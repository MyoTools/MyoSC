#!/bin/bash
# Single-gpu training (default: no validation, last ckpt = work_dirs/MyoPWC/latest.pth)
python tools/train.py local_configs/segformer/B1/segformer.b1.512x512.ade.160k.py --gpus 1

# With validation during training:
# python tools/train.py local_configs/segformer/B1/segformer.b1.512x512.ade.160k.py --gpus 1 --validate

# Multi-gpu training
# ./tools/dist_train.sh local_configs/segformer/B1/segformer.b1.512x512.ade.160k.py <GPU_NUM>
