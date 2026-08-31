"""MyoHLS instances + SegFormer pixels -> per-fiber majority class.

Each {stem}_cp_masks.png is fused with {stem}.png in the same folder.
Pixels 1/2/3 are counted; 0 is ignored. The class map is overwritten;
the original SegFormer map is kept as {stem}_segformer_raw.png.

    python scripts/fuse.py --pred_dir <PRED_DIR>
"""
from __future__ import annotations

import argparse
from pathlib import Path

import numpy as np
from PIL import Image


def fuse_majority_in_dir(pred_dir: Path, nclasses: int = 4) -> None:
    mask_files = sorted(pred_dir.glob("*_cp_masks.png"))
    if not mask_files:
        raise SystemExit(f"No *_cp_masks.png in {pred_dir}")
    for mask_path in mask_files:
        stem = mask_path.name[: -len("_cp_masks.png")]
        class_path = pred_dir / f"{stem}.png"
        if not class_path.is_file():
            print(f"skip fuse {stem}: missing class png")
            continue
        inst = np.array(Image.open(mask_path))
        pixel_class = np.array(Image.open(class_path))
        raw_path = pred_dir / f"{stem}_segformer_raw.png"
        if not raw_path.is_file():
            Image.fromarray(pixel_class.astype(np.uint8)).save(raw_path)
        out_class = np.zeros(inst.shape, dtype=np.uint8)
        n_drop = 0
        for i in range(1, int(inst.max()) + 1):
            sel = inst == i
            if not np.any(sel):
                continue
            vals = pixel_class[sel].astype(np.int64)
            fg = vals[(vals > 0) & (vals < nclasses)]
            if fg.size:
                out_class[sel] = int(np.bincount(fg, minlength=nclasses).argmax())
            else:
                n_drop += 1
        Image.fromarray(out_class).save(class_path)
        print(f"fuse {stem}: fibers={int(inst.max())} empty-class={n_drop}")
    print(f"fused class maps in {pred_dir}")


def main():
    p = argparse.ArgumentParser(
        description="Majority-vote fusion of Cellpose instances and SegFormer classes."
    )
    p.add_argument(
        "--pred_dir",
        type=Path,
        required=True,
        help="Folder with {stem}_cp_masks.png and {stem}.png",
    )
    args = p.parse_args()
    fuse_majority_in_dir(args.pred_dir)


if __name__ == "__main__":
    main()
