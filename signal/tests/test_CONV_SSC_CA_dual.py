import sys
sys.path.insert(0, "../../")
import signal
from CONV import SevenSegmentConverterCADual


class TestSevenSegmentConverterCADual:
    def test_CONV_SSC_CA_dual_000000000(self):
        ssd = SevenSegmentConverterCADual(0, 0, 0, 0, 0, 0, 0, 0, 0)
        assert ssd.get_output() == (1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1)

    def test_CONV_SSC_CA_dual_011111111(self):
        ssd = SevenSegmentConverterCADual(0, 1, 1, 1, 1, 1, 1, 1, 1)
        assert ssd.get_output() == (1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1)

    def test_CONV_SSC_CA_dual_100000000(self):
        ssd = SevenSegmentConverterCADual(1, 0, 0, 0, 0, 0, 0, 0, 0)
        assert ssd.get_output() == (1, 0, 0, 0, 0, 0, 0, 1, 0, 0, 0, 0, 0, 0)

    def test_CONV_SSC_CA_dual_100001111(self):
        ssd = SevenSegmentConverterCADual(1, 0, 0, 0, 0, 1, 1, 1, 1)
        assert ssd.get_output() == (1, 0, 0, 0, 0, 0, 0, 0, 0, 0, 1, 1, 1, 0)

    def test_CONV_SSC_CA_dual_111110000(self):
        ssd = SevenSegmentConverterCADual(1, 1, 1, 1, 1, 0, 0, 0, 0)
        assert ssd.get_output() == (0, 0, 0, 1, 1, 1, 0, 1, 0, 0, 0, 0, 0, 0)

    def test_CONV_SSC_CA_dual_111111111(self):
        ssd = SevenSegmentConverterCADual(1, 1, 1, 1, 1, 1, 1, 1, 1)
        assert ssd.get_output() == (0, 0, 0, 1, 1, 1, 0, 0, 0, 0, 1, 1, 1, 0)
