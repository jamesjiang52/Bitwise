import sys
sys.path.append("..")
from CONV import CONVHEXToCCSSD


class TestCONVHEXToCCSSD:
    def test_CONV_HEXtoCCSSD_00000(self):
        ssd = CONVHEXToCCSSD(0, 0, 0, 0, 0)
        assert ssd.get_output() == (0, 0, 0, 0, 0, 0, 0)

    def test_CONV_HEXtoCCSSD_01111(self):
        ssd = CONVHEXToCCSSD(0, 1, 1, 1, 1)
        assert ssd.get_output() == (0, 0, 0, 0, 0, 0, 0)

    def test_CONV_HEXToCCSSD_10000(self):
        ssd = CONVHEXToCCSSD(1, 0, 0, 0, 0)
        assert ssd.get_output() == (0, 1, 1, 1, 1, 1, 1)

    def test_CONV_HEXToCCSSD_10001(self):
        ssd = CONVHEXToCCSSD(1, 0, 0, 0, 1)
        assert ssd.get_output() == (0, 0, 0, 0, 1, 1, 0)

    def test_CONV_HEXToCCSSD_10010(self):
        ssd = CONVHEXToCCSSD(1, 0, 0, 1, 0)
        assert ssd.get_output() == (1, 0, 1, 1, 0, 1, 1)

    def test_CONV_HEXToCCSSD_10011(self):
        ssd = CONVHEXToCCSSD(1, 0, 0, 1, 1)
        assert ssd.get_output() == (1, 0, 0, 1, 1, 1, 1)

    def test_CONV_HEXToCCSSD_10100(self):
        ssd = CONVHEXToCCSSD(1, 0, 1, 0, 0)
        assert ssd.get_output() == (1, 1, 0, 0, 1, 1, 0)

    def test_CONV_HEXToCCSSD_10101(self):
        ssd = CONVHEXToCCSSD(1, 0, 1, 0, 1)
        assert ssd.get_output() == (1, 1, 0, 1, 1, 0, 1)

    def test_CONV_HEXToCCSSD_10110(self):
        ssd = CONVHEXToCCSSD(1, 0, 1, 1, 0)
        assert ssd.get_output() == (1, 1, 1, 1, 1, 0, 1)

    def test_CONV_HEXToCCSSD_10111(self):
        ssd = CONVHEXToCCSSD(1, 0, 1, 1, 1)
        assert ssd.get_output() == (0, 0, 0, 0, 1, 1, 1)

    def test_CONV_HEXToCCSSD_11000(self):
        ssd = CONVHEXToCCSSD(1, 1, 0, 0, 0)
        assert ssd.get_output() == (1, 1, 1, 1, 1, 1, 1)

    def test_CONV_HEXToCCSSD_11001(self):
        ssd = CONVHEXToCCSSD(1, 1, 0, 0, 1)
        assert ssd.get_output() == (1, 1, 0, 1, 1, 1, 1)

    def test_CONV_HEXToCCSSD_11010(self):
        ssd = CONVHEXToCCSSD(1, 1, 0, 1, 0)
        assert ssd.get_output() == (1, 1, 1, 0, 1, 1, 1)

    def test_CONV_HEXToCCSSD_11011(self):
        ssd = CONVHEXToCCSSD(1, 1, 0, 1, 1)
        assert ssd.get_output() == (1, 1, 1, 1, 1, 0, 0)

    def test_CONV_HEXToCCSSD_11100(self):
        ssd = CONVHEXToCCSSD(1, 1, 1, 0, 0)
        assert ssd.get_output() == (0, 1, 1, 1, 0, 0, 1)

    def test_CONV_HEXToCCSSD_11101(self):
        ssd = CONVHEXToCCSSD(1, 1, 1, 0, 1)
        assert ssd.get_output() == (1, 0, 1, 1, 1, 1, 0)

    def test_CONV_HEXToCCSSD_11110(self):
        ssd = CONVHEXToCCSSD(1, 1, 1, 1, 0)
        assert ssd.get_output() == (1, 1, 1, 1, 0, 0, 1)

    def test_CONV_HEXToCCSSD_11111(self):
        ssd = CONVHEXToCCSSD(1, 1, 1, 1, 1)
        assert ssd.get_output() == (1, 1, 1, 0, 0, 0, 1)
