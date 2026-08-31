from argparse import ArgumentParser
import os
import glob

from mmseg.apis import inference_segmentor, init_segmentor, show_result_pyplot
from mmseg.core.evaluation import get_palette

def process_images_in_folder(input_folder, config, checkpoint, device, palette):
    # Get all image files in the folder (assuming they have common image extensions)
    image_files = glob.glob(os.path.join(input_folder, '*.jpg')) + \
                  glob.glob(os.path.join(input_folder, '*.png')) + \
                  glob.glob(os.path.join(input_folder, '*.tif'))

    # Load the model
    model = init_segmentor(config, checkpoint, device=device)

    # Process each image
    for img_path in image_files:
        print(f"Processing image: {img_path}")
        result = inference_segmentor(model, img_path)
        
        # Show the result
        show_result_pyplot(model, img_path, result, get_palette(palette))


def main():
    parser = ArgumentParser()
    parser.add_argument('input_folder', help='Folder containing images')
    parser.add_argument('config', help='Config file')
    parser.add_argument('checkpoint', help='Checkpoint file')
    parser.add_argument(
        '--device', default='cuda:0', help='Device used for inference')
    parser.add_argument(
        '--palette', default='ade', help='Color palette used for segmentation map')
    args = parser.parse_args()

    # Process all images in the folder
    process_images_in_folder(args.input_folder, args.config, args.checkpoint, args.device, args.palette)

if __name__ == "__main__":
    main()
