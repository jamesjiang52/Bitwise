import sys
sys.path.insert(0, "../../")
import signal
from CONV import SevenSegmentConverterCCQuad


class TestSevenSegmentConverterCCQuad:
    def test_CONV_SSC_CC_quad_00000000000000000(self):
        ssd = SevenSegmentConverterCCQuad(
            0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0)
        assert ssd.get_output() == (
            0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,
            0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0)

    def test_CONV_SSC_CC_quad_01111111111111111(self):
        ssd = SevenSegmentConverterCCQuad(
            0, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1)
        assert ssd.get_output() == (
            0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,
            0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0)

    def test_CONV_SSC_CC_quad_10000000000000000(self):
        ssd = SevenSegmentConverterCCQuad(
            1, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0)
        assert ssd.get_output() == (
            0, 1, 1, 1, 1, 1, 1, 0, 1, 1, 1, 1, 1, 1,
            0, 1, 1, 1, 1, 1, 1, 0, 1, 1, 1, 1, 1, 1)

    def test_CONV_SSC_CC_quad_10000000000001111(self):
        ssd = SevenSegmentConverterCCQuad(
            1, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 1, 1, 1, 1)
        assert ssd.get_output() == (
            0, 1, 1, 1, 1, 1, 1, 0, 1, 1, 1, 1, 1, 1,
            0, 1, 1, 1, 1, 1, 1, 1, 1, 1, 0, 0, 0, 1)

    def test_CONV_SSC_CC_quad_10000000011110000(self):
        ssd = SevenSegmentConverterCCQuad(
            1, 0, 0, 0, 0, 0, 0, 0, 0, 1, 1, 1, 1, 0, 0, 0, 0)
        assert ssd.get_output() == (
            0, 1, 1, 1, 1, 1, 1, 0, 1, 1, 1, 1, 1, 1,
            1, 1, 1, 0, 0, 0, 1, 0, 1, 1, 1, 1, 1, 1)

    def test_CONV_SSC_CC_quad_10000111100000000(self):
        ssd = SevenSegmentConverterCCQuad(
            1, 0, 0, 0, 0, 1, 1, 1, 1, 0, 0, 0, 0, 0, 0, 0, 0)
        assert ssd.get_output() == (
            0, 1, 1, 1, 1, 1, 1, 1, 1, 1, 0, 0, 0, 1,
            0, 1, 1, 1, 1, 1, 1, 0, 1, 1, 1, 1, 1, 1)

    def test_CONV_SSC_CC_quad_11111000000000000(self):
        ssd = SevenSegmentConverterCCQuad(
            1, 1, 1, 1, 1, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0)
        assert ssd.get_output() == (
            1, 1, 1, 0, 0, 0, 1, 0, 1, 1, 1, 1, 1, 1,
            0, 1, 1, 1, 1, 1, 1, 0, 1, 1, 1, 1, 1, 1)

    def test_CONV_SSC_CC_quad_11111111111111111(self):
        ssd = SevenSegmentConverterCCQuad(
            1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1)
        assert ssd.get_output() == (
            1, 1, 1, 0, 0, 0, 1, 1, 1, 1, 0, 0, 0, 1,
            1, 1, 1, 0, 0, 0, 1, 1, 1, 1, 0, 0, 0, 1)