from argparse import ArgumentParser
import os
import glob
import cv2
import numpy as np

from mmseg.apis import inference_segmentor, init_segmentor


def process_images_in_folder(input_folder, config, checkpoint, device, palette, output_dir="results"):
    image_files = glob.glob(os.path.join(input_folder, '*.jpg')) + \
                  glob.glob(os.path.join(input_folder, '*.png')) + \
                  glob.glob(os.path.join(input_folder, '*.tif'))

    os.makedirs(output_dir, exist_ok=True)
    print(f"Output will be saved to: {output_dir}")

    model = init_segmentor(config, checkpoint, device=device)

    for img_path in image_files:
        print(f"Processing image: {img_path}")
        result = inference_segmentor(model, img_path)

        # Class IDs: 0 background, 1 Type I, 2 Type IIa, 3 Type IIb
        class_mask = np.asarray(result[0], dtype=np.uint8)

        base_name_no_ext, _ = os.path.splitext(os.path.basename(img_path))
        output_path = os.path.join(output_dir, base_name_no_ext + ".png")
        cv2.imwrite(output_path, class_mask)
        uniq = np.unique(class_mask)
        print(f"Saved class mask to: {output_path} unique={uniq.tolist()}")


def main():
    parser = ArgumentParser()
    parser.add_argument('input_folder', help='Folder containing images (e.g., pross_img)')
    parser.add_argument('config', help='Config file')
    parser.add_argument('checkpoint', help='Checkpoint file')
    parser.add_argument(
        '--device', default='cuda:0', help='Device used for inference')
    parser.add_argument(
        '--palette', default='ade', help='Color palette (not used)')
    parser.add_argument(
        '--out-dir', default='results', help='Folder for 0/1/2/3 class PNGs')
    args = parser.parse_args()

    process_images_in_folder(
        args.input_folder, args.config, args.checkpoint, args.device, args.palette,
        output_dir=args.out_dir)


if __name__ == "__main__":
    main()
