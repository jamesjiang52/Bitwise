import sys
sys.path.insert(0, "../../")
import signal
from CONV import SevenSegmentConverterCA


class TestSevenSegmentConverterCA:
    def test_CONV_SSC_CA_00000(self):
        ssd = SevenSegmentConverterCA(0, 0, 0, 0, 0)
        assert ssd.get_output() == (1, 1, 1, 1, 1, 1, 1)

    def test_CONV_SSC_CA_01111(self):
        ssd = SevenSegmentConverterCA(0, 1, 1, 1, 1)
        assert ssd.get_output() == (1, 1, 1, 1, 1, 1, 1)

    def test_CONV_SSC_CA_10000(self):
        ssd = SevenSegmentConverterCA(1, 0, 0, 0, 0)
        assert ssd.get_output() == (1, 0, 0, 0, 0, 0, 0)

    def test_CONV_SSC_CA_10001(self):
        ssd = SevenSegmentConverterCA(1, 0, 0, 0, 1)
        assert ssd.get_output() == (1, 1, 1, 1, 0, 0, 1)

    def test_CONV_SSC_CA_10010(self):
        ssd = SevenSegmentConverterCA(1, 0, 0, 1, 0)
        assert ssd.get_output() == (0, 1, 0, 0, 1, 0, 0)

    def test_CONV_SSC_CA_10011(self):
        ssd = SevenSegmentConverterCA(1, 0, 0, 1, 1)
        assert ssd.get_output() == (0, 1, 1, 0, 0, 0, 0)

    def test_CONV_SSC_CA_10100(self):
        ssd = SevenSegmentConverterCA(1, 0, 1, 0, 0)
        assert ssd.get_output() == (0, 0, 1, 1, 0, 0, 1)

    def test_CONV_SSC_CA_10101(self):
        ssd = SevenSegmentConverterCA(1, 0, 1, 0, 1)
        assert ssd.get_output() == (0, 0, 1, 0, 0, 1, 0)

    def test_CONV_SSC_CA_10110(self):
        ssd = SevenSegmentConverterCA(1, 0, 1, 1, 0)
        assert ssd.get_output() == (0, 0, 0, 0, 0, 1, 0)

    def test_CONV_SSC_CA_10111(self):
        ssd = SevenSegmentConverterCA(1, 0, 1, 1, 1)
        assert ssd.get_output() == (1, 1, 1, 1, 0, 0, 0)

    def test_CONV_SSC_CA_11000(self):
        ssd = SevenSegmentConverterCA(1, 1, 0, 0, 0)
        assert ssd.get_output() == (0, 0, 0, 0, 0, 0, 0)

    def test_CONV_SSC_CA_11001(self):
        ssd = SevenSegmentConverterCA(1, 1, 0, 0, 1)
        assert ssd.get_output() == (0, 0, 1, 0, 0, 0, 0)

    def test_CONV_SSC_CA_11010(self):
        ssd = SevenSegmentConverterCA(1, 1, 0, 1, 0)
        assert ssd.get_output() == (0, 0, 0, 1, 0, 0, 0)

    def test_CONV_SSC_CA_11011(self):
        ssd = SevenSegmentConverterCA(1, 1, 0, 1, 1)
        assert ssd.get_output() == (0, 0, 0, 0, 0, 1, 1)

    def test_CONV_SSC_CA_11100(self):
        ssd = SevenSegmentConverterCA(1, 1, 1, 0, 0)
        assert ssd.get_output() == (1, 0, 0, 0, 1, 1, 0)

    def test_CONV_SSC_CA_11101(self):
        ssd = SevenSegmentConverterCA(1, 1, 1, 0, 1)
        assert ssd.get_output() == (0, 1, 0, 0, 0, 0, 1)

    def test_CONV_SSC_CA_11110(self):
        ssd = SevenSegmentConverterCA(1, 1, 1, 1, 0)
        assert ssd.get_output() == (0, 0, 0, 0, 1, 1, 0)

    def test_CONV_SSC_CA_11111(self):
        ssd = SevenSegmentConverterCA(1, 1, 1, 1, 1)
        assert ssd.get_output() == (0, 0, 0, 1, 1, 1, 0)
