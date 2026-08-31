from .builder import DATASETS
from .custom import CustomDataset


@DATASETS.register_module()
class ADE20KDataset(CustomDataset):
    """ADE20K dataset.

    Muscle-fiber semantic labels: 0 background + 3 cell types.
    ``reduce_zero_label`` is False so background stays class 0.
    Images are ``.tif``, masks are ``.png``.
    """
    CLASSES = (
        'background', 'cell1', 'cell2', 'cell3'
        )

    PALETTE = [[0, 0, 0], [120, 120, 120], [180, 120, 120], [6, 230, 230]]

    def __init__(self, **kwargs):
        super(ADE20KDataset, self).__init__(
            img_suffix='.tif',
            seg_map_suffix='.png',
            reduce_zero_label=False,
            **kwargs)
