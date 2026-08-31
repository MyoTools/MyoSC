# MyoSC

Instance segmentation and typing of myofibers on sequential mATPase/SDH images: **MyoHLS** (Cellpose-SAM) predicts contours, **MyoPWC** (SegFormer-B1) predicts pixel classes, and majority vote inside each instance yields Type I / IIa / IIb.

This repository is code only. Download the two fine-tuned weights from the cloud drive linked in the paper (or from the corresponding author).

## 1. Weights

| File | Place it at |
|---|---|
| MyoHLS (Cellpose model, no extension) | `%USERPROFILE%\.cellpose\models\MyoHLS` (Linux/macOS: `~/.cellpose/models/MyoHLS`) |
| MyoPWC `latest.pth` | any path; referred to below as `MyoPWC.pth` |

## 2. Environments

```text
conda env create -f envs/environment_cpsam.yml
conda activate cpsam408
python -m pip install torch torchvision --index-url https://download.pytorch.org/whl/cu126
```

For SegFormer, follow the comments in `envs/environment_segformer.yml` (PyTorch 1.7.1+cu110, `MMCV_WITH_OPS=0`, `mmcv==1.2.7`), then:

```text
conda activate segformer
cd myopwc/SegFormer
python -m pip install -e .
```

Fusion only needs `pip install pillow numpy` in any environment.

## 3. Inference and fusion

Input: RGB `tif` / `png` / `jpg`. Class pixels **0 / 1 / 2 / 3** = background / Type I / IIa / IIb.

**1. Contours** (`cpsam408`)

```text
python -m cellpose --use_gpu --verbose --dir <IMAGE_DIR> --pretrained_model MyoHLS --augment --exclude_on_edges --save_png --no_npy --savedir <PRED_DIR>
```

Writes `{stem}_cp_masks.png`.

**2. Pixel classes** (`segformer`)

```text
python myopwc/SegFormer/demo/image_demo.py <IMAGE_DIR> myopwc/SegFormer/local_configs/segformer/B1/segformer.b1.512x512.ade.160k.py <MyoPWC.pth> --device cuda:0 --out-dir <PRED_DIR>
```

Writes `{stem}.png`. Both outputs must share `<PRED_DIR>` and the same `{stem}` as the image.

**3. Fusion**

```text
python scripts/fuse.py --pred_dir <PRED_DIR>
```

Overwrites `{stem}.png` with the per-instance majority class (ignores 0). The raw SegFormer map is saved as `{stem}_segformer_raw.png`.

Training commands: `myohls/cli.txt`. SegFormer training needs `pretrained/mit_b1.pth` and a `data_root` in the config. `myopwc/SegFormer` is a modified NVIDIA SegFormer; see that folder’s `LICENSE`.
