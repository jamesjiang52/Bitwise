import sys
sys.path.insert(0, "../../")
import signal
from CONV import CONVHEXToCCSSD


class TestCONVHEXToCCSSD:
    def test_CONV_HEXtoCCSSD_00000(self):
        ssd = CONVHEXToCCSSD(0, 0, 0, 0, 0)
        assert ssd.get_output() == (0, 0, 0, 0, 0, 0, 0)

    def test_CONV_HEXtoCCSSD_01111(self):
        ssd = CONVHEXToCCSSD(0, 1, 1, 1, 1)
        assert ssd.get_output() == (0, 0, 0, 0, 0, 0, 0)

    def test_CONV_HEXtoCCSSD_10000(self):
        ssd = CONVHEXToCCSSD(1, 0, 0, 0, 0)
        assert ssd.get_output() == (0, 1, 1, 1, 1, 1, 1)

    def test_CONV_HEXtoCCSSD_10001(self):
        ssd = CONVHEXToCCSSD(1, 0, 0, 0, 1)
        assert ssd.get_output() == (0, 0, 0, 0, 1, 1, 0)

    def test_CONV_HEXtoCCSSD_10010(self):
        ssd = CONVHEXToCCSSD(1, 0, 0, 1, 0)
        assert ssd.get_output() == (1, 0, 1, 1, 0, 1, 1)

    def test_CONV_HEXtoCCSSD_10011(self):
        ssd = CONVHEXToCCSSD(1, 0, 0, 1, 1)
        assert ssd.get_output() == (1, 0, 0, 1, 1, 1, 1)

    def test_CONV_HEXtoCCSSD_10100(self):
        ssd = CONVHEXToCCSSD(1, 0, 1, 0, 0)
        assert ssd.get_output() == (1, 1, 0, 0, 1, 1, 0)

    def test_CONV_HEXtoCCSSD_10101(self):
        ssd = CONVHEXToCCSSD(1, 0, 1, 0, 1)
        assert ssd.get_output() == (1, 1, 0, 1, 1, 0, 1)

    def test_CONV_HEXtoCCSSD_10110(self):
        ssd = CONVHEXToCCSSD(1, 0, 1, 1, 0)
        assert ssd.get_output() == (1, 1, 1, 1, 1, 0, 1)

    def test_CONV_HEXtoCCSSD_10111(self):
        ssd = CONVHEXToCCSSD(1, 0, 1, 1, 1)
        assert ssd.get_output() == (0, 0, 0, 0, 1, 1, 1)

    def test_CONV_HEXtoCCSSD_11000(self):
        ssd = CONVHEXToCCSSD(1, 1, 0, 0, 0)
        assert ssd.get_output() == (1, 1, 1, 1, 1, 1, 1)

    def test_CONV_HEXtoCCSSD_11001(self):
        ssd = CONVHEXToCCSSD(1, 1, 0, 0, 1)
        assert ssd.get_output() == (1, 1, 0, 1, 1, 1, 1)

    def test_CONV_HEXtoCCSSD_11010(self):
        ssd = CONVHEXToCCSSD(1, 1, 0, 1, 0)
        assert ssd.get_output() == (1, 1, 1, 0, 1, 1, 1)

    def test_CONV_HEXtoCCSSD_11011(self):
        ssd = CONVHEXToCCSSD(1, 1, 0, 1, 1)
        assert ssd.get_output() == (1, 1, 1, 1, 1, 0, 0)

    def test_CONV_HEXtoCCSSD_11100(self):
        ssd = CONVHEXToCCSSD(1, 1, 1, 0, 0)
        assert ssd.get_output() == (0, 1, 1, 1, 0, 0, 1)

    def test_CONV_HEXtoCCSSD_11101(self):
        ssd = CONVHEXToCCSSD(1, 1, 1, 0, 1)
        assert ssd.get_output() == (1, 0, 1, 1, 1, 1, 0)

    def test_CONV_HEXtoCCSSD_11110(self):
        ssd = CONVHEXToCCSSD(1, 1, 1, 1, 0)
        assert ssd.get_output() == (1, 1, 1, 1, 0, 0, 1)

    def test_CONV_HEXtoCCSSD_11111(self):
        ssd = CONVHEXToCCSSD(1, 1, 1, 1, 1)
        assert ssd.get_output() == (1, 1, 1, 0, 0, 0, 1)
