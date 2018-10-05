import sys
sys.path.insert(0, "../../")
import signal
from CONV import SevenSegmentConverterCCDual


class TestSevenSegmentConverterCCDual:
    def test_CONV_SSC_CC_dual_000000000(self):
        ssd = SevenSegmentConverterCCDual(0, 0, 0, 0, 0, 0, 0, 0, 0)
        assert ssd.get_output() == (0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0)

    def test_CONV_SSC_CC_dual_011111111(self):
        ssd = SevenSegmentConverterCCDual(0, 1, 1, 1, 1, 1, 1, 1, 1)
        assert ssd.get_output() == (0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0)

    def test_CONV_SSC_CC_dual_100000000(self):
        ssd = SevenSegmentConverterCCDual(1, 0, 0, 0, 0, 0, 0, 0, 0)
        assert ssd.get_output() == (0, 1, 1, 1, 1, 1, 1, 0, 1, 1, 1, 1, 1, 1)

    def test_CONV_SSC_CC_dual_100001111(self):
        ssd = SevenSegmentConverterCCDual(1, 0, 0, 0, 0, 1, 1, 1, 1)
        assert ssd.get_output() == (0, 1, 1, 1, 1, 1, 1, 1, 1, 1, 0, 0, 0, 1)

    def test_CONV_SSC_CC_dual_111110000(self):
        ssd = SevenSegmentConverterCCDual(1, 1, 1, 1, 1, 0, 0, 0, 0)
        assert ssd.get_output() == (1, 1, 1, 0, 0, 0, 1, 0, 1, 1, 1, 1, 1, 1)

    def test_CONV_SSC_CC_dual_111111111(self):
        ssd = SevenSegmentConverterCCDual(1, 1, 1, 1, 1, 1, 1, 1, 1)
        assert ssd.get_output() == (1, 1, 1, 0, 0, 0, 1, 1, 1, 1, 0, 0, 0, 1)
