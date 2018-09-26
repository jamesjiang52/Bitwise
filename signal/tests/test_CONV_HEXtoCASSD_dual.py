import sys
sys.path.append("..")
from CONV import CONVHEXToCASSDDual


class TestCONVHEXToCASSDDual:
    def test_CONV_HEXtoCASSD_dual_000000000(self):
        ssd = CONVHEXToCASSDDual(0, 0, 0, 0, 0, 0, 0, 0, 0)
        assert ssd.get_output() == (1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1)

    def test_CONV_HEXtoCASSD_dual_011111111(self):
        ssd = CONVHEXToCASSDDual(0, 1, 1, 1, 1, 1, 1, 1, 1)
        assert ssd.get_output() == (1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1)

    def test_CONV_HEXtoCASSD_dual_100000000(self):
        ssd = CONVHEXToCASSDDual(1, 0, 0, 0, 0, 0, 0, 0, 0)
        assert ssd.get_output() == (1, 0, 0, 0, 0, 0, 0, 1, 0, 0, 0, 0, 0, 0)

    def test_CONV_HEXtoCASSD_dual_100001111(self):
        ssd = CONVHEXToCASSDDual(1, 0, 0, 0, 0, 1, 1, 1, 1)
        assert ssd.get_output() == (1, 0, 0, 0, 0, 0, 0, 0, 0, 0, 1, 1, 1, 0)

    def test_CONV_HEXtoCASSD_dual_111110000(self):
        ssd = CONVHEXToCASSDDual(1, 1, 1, 1, 1, 0, 0, 0, 0)
        assert ssd.get_output() == (0, 0, 0, 1, 1, 1, 0, 1, 0, 0, 0, 0, 0, 0)

    def test_CONV_HEXtoCASSD_dual_111111111(self):
        ssd = CONVHEXToCASSDDual(1, 1, 1, 1, 1, 1, 1, 1, 1)
        assert ssd.get_output() == (0, 0, 0, 1, 1, 1, 0, 0, 0, 0, 1, 1, 1, 0)
