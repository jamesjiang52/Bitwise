import sys
sys.path.insert(0, "../../")
import signal
from CONV import SevenSegmentConverterCC


class TestSevenSegmentConverterCC:
    def test_CONV_SSC_CC_00000(self):
        ssd = SevenSegmentConverterCC(0, 0, 0, 0, 0)
        assert ssd.get_output() == (0, 0, 0, 0, 0, 0, 0)

    def test_CONV_SSC_CC_01111(self):
        ssd = SevenSegmentConverterCC(0, 1, 1, 1, 1)
        assert ssd.get_output() == (0, 0, 0, 0, 0, 0, 0)

    def test_CONV_SSC_CC_10000(self):
        ssd = SevenSegmentConverterCC(1, 0, 0, 0, 0)
        assert ssd.get_output() == (0, 1, 1, 1, 1, 1, 1)

    def test_CONV_SSC_CC_10001(self):
        ssd = SevenSegmentConverterCC(1, 0, 0, 0, 1)
        assert ssd.get_output() == (0, 0, 0, 0, 1, 1, 0)

    def test_CONV_SSC_CC_10010(self):
        ssd = SevenSegmentConverterCC(1, 0, 0, 1, 0)
        assert ssd.get_output() == (1, 0, 1, 1, 0, 1, 1)

    def test_CONV_SSC_CC_10011(self):
        ssd = SevenSegmentConverterCC(1, 0, 0, 1, 1)
        assert ssd.get_output() == (1, 0, 0, 1, 1, 1, 1)

    def test_CONV_SSC_CC_10100(self):
        ssd = SevenSegmentConverterCC(1, 0, 1, 0, 0)
        assert ssd.get_output() == (1, 1, 0, 0, 1, 1, 0)

    def test_CONV_SSC_CC_10101(self):
        ssd = SevenSegmentConverterCC(1, 0, 1, 0, 1)
        assert ssd.get_output() == (1, 1, 0, 1, 1, 0, 1)

    def test_CONV_SSC_CC_10110(self):
        ssd = SevenSegmentConverterCC(1, 0, 1, 1, 0)
        assert ssd.get_output() == (1, 1, 1, 1, 1, 0, 1)

    def test_CONV_SSC_CC_10111(self):
        ssd = SevenSegmentConverterCC(1, 0, 1, 1, 1)
        assert ssd.get_output() == (0, 0, 0, 0, 1, 1, 1)

    def test_CONV_SSC_CC_11000(self):
        ssd = SevenSegmentConverterCC(1, 1, 0, 0, 0)
        assert ssd.get_output() == (1, 1, 1, 1, 1, 1, 1)

    def test_CONV_SSC_CC_11001(self):
        ssd = SevenSegmentConverterCC(1, 1, 0, 0, 1)
        assert ssd.get_output() == (1, 1, 0, 1, 1, 1, 1)

    def test_CONV_SSC_CC_11010(self):
        ssd = SevenSegmentConverterCC(1, 1, 0, 1, 0)
        assert ssd.get_output() == (1, 1, 1, 0, 1, 1, 1)

    def test_CONV_SSC_CC_11011(self):
        ssd = SevenSegmentConverterCC(1, 1, 0, 1, 1)
        assert ssd.get_output() == (1, 1, 1, 1, 1, 0, 0)

    def test_CONV_SSC_CC_11100(self):
        ssd = SevenSegmentConverterCC(1, 1, 1, 0, 0)
        assert ssd.get_output() == (0, 1, 1, 1, 0, 0, 1)

    def test_CONV_SSC_CC_11101(self):
        ssd = SevenSegmentConverterCC(1, 1, 1, 0, 1)
        assert ssd.get_output() == (1, 0, 1, 1, 1, 1, 0)

    def test_CONV_SSC_CC_11110(self):
        ssd = SevenSegmentConverterCC(1, 1, 1, 1, 0)
        assert ssd.get_output() == (1, 1, 1, 1, 0, 0, 1)

    def test_CONV_SSC_CC_11111(self):
        ssd = SevenSegmentConverterCC(1, 1, 1, 1, 1)
        assert ssd.get_output() == (1, 1, 1, 0, 0, 0, 1)
