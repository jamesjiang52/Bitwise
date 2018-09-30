import sys
sys.path.insert(0, "../../")
import signal
from CONV import CONVHEXToCASSD


class TestCONVHEXToCASSD:
    def test_CONV_HEXtoCASSD_00000(self):
        ssd = CONVHEXToCASSD(0, 0, 0, 0, 0)
        assert ssd.get_output() == (1, 1, 1, 1, 1, 1, 1)

    def test_CONV_HEXtoCASSD_01111(self):
        ssd = CONVHEXToCASSD(0, 1, 1, 1, 1)
        assert ssd.get_output() == (1, 1, 1, 1, 1, 1, 1)

    def test_CONV_HEXtoCASSD_10000(self):
        ssd = CONVHEXToCASSD(1, 0, 0, 0, 0)
        assert ssd.get_output() == (1, 0, 0, 0, 0, 0, 0)

    def test_CONV_HEXtoCASSD_10001(self):
        ssd = CONVHEXToCASSD(1, 0, 0, 0, 1)
        assert ssd.get_output() == (1, 1, 1, 1, 0, 0, 1)

    def test_CONV_HEXtoCASSD_10010(self):
        ssd = CONVHEXToCASSD(1, 0, 0, 1, 0)
        assert ssd.get_output() == (0, 1, 0, 0, 1, 0, 0)

    def test_CONV_HEXtoCASSD_10011(self):
        ssd = CONVHEXToCASSD(1, 0, 0, 1, 1)
        assert ssd.get_output() == (0, 1, 1, 0, 0, 0, 0)

    def test_CONV_HEXtoCASSD_10100(self):
        ssd = CONVHEXToCASSD(1, 0, 1, 0, 0)
        assert ssd.get_output() == (0, 0, 1, 1, 0, 0, 1)

    def test_CONV_HEXtoCASSD_10101(self):
        ssd = CONVHEXToCASSD(1, 0, 1, 0, 1)
        assert ssd.get_output() == (0, 0, 1, 0, 0, 1, 0)

    def test_CONV_HEXtoCASSD_10110(self):
        ssd = CONVHEXToCASSD(1, 0, 1, 1, 0)
        assert ssd.get_output() == (0, 0, 0, 0, 0, 1, 0)

    def test_CONV_HEXtoCASSD_10111(self):
        ssd = CONVHEXToCASSD(1, 0, 1, 1, 1)
        assert ssd.get_output() == (1, 1, 1, 1, 0, 0, 0)

    def test_CONV_HEXtoCASSD_11000(self):
        ssd = CONVHEXToCASSD(1, 1, 0, 0, 0)
        assert ssd.get_output() == (0, 0, 0, 0, 0, 0, 0)

    def test_CONV_HEXtoCASSD_11001(self):
        ssd = CONVHEXToCASSD(1, 1, 0, 0, 1)
        assert ssd.get_output() == (0, 0, 1, 0, 0, 0, 0)

    def test_CONV_HEXtoCASSD_11010(self):
        ssd = CONVHEXToCASSD(1, 1, 0, 1, 0)
        assert ssd.get_output() == (0, 0, 0, 1, 0, 0, 0)

    def test_CONV_HEXtoCASSD_11011(self):
        ssd = CONVHEXToCASSD(1, 1, 0, 1, 1)
        assert ssd.get_output() == (0, 0, 0, 0, 0, 1, 1)

    def test_CONV_HEXtoCASSD_11100(self):
        ssd = CONVHEXToCASSD(1, 1, 1, 0, 0)
        assert ssd.get_output() == (1, 0, 0, 0, 1, 1, 0)

    def test_CONV_HEXtoCASSD_11101(self):
        ssd = CONVHEXToCASSD(1, 1, 1, 0, 1)
        assert ssd.get_output() == (0, 1, 0, 0, 0, 0, 1)

    def test_CONV_HEXtoCASSD_11110(self):
        ssd = CONVHEXToCASSD(1, 1, 1, 1, 0)
        assert ssd.get_output() == (0, 0, 0, 0, 1, 1, 0)

    def test_CONV_HEXtoCASSD_11111(self):
        ssd = CONVHEXToCASSD(1, 1, 1, 1, 1)
        assert ssd.get_output() == (0, 0, 0, 1, 1, 1, 0)
