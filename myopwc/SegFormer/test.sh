#!/bin/bash
python demo/image_demo.py pross_img/ local_configs/segformer/B1/segformer.b1.512x512.ade.160k.py \
./work_dirs/MyoPWC/latest.pth --device cuda:0
