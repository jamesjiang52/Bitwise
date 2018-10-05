import sys
sys.path.insert(0, "../../")
import signal
from CONV import SevenSegmentConverterCAQuad


class TestSevenSegmentConverterCAQuad:
    def test_CONV_SSC_CA_quad_00000000000000000(self):
        ssd = SevenSegmentConverterCAQuad(
            0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0)
        assert ssd.get_output() == (
            1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1,
            1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1)

    def test_CONV_SSC_CA_quad_01111111111111111(self):
        ssd = SevenSegmentConverterCAQuad(
            0, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1)
        assert ssd.get_output() == (
            1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1,
            1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1)

    def test_CONV_SSC_CA_quad_10000000000000000(self):
        ssd = SevenSegmentConverterCAQuad(
            1, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0)
        assert ssd.get_output() == (
            1, 0, 0, 0, 0, 0, 0, 1, 0, 0, 0, 0, 0, 0,
            1, 0, 0, 0, 0, 0, 0, 1, 0, 0, 0, 0, 0, 0)

    def test_CONV_SSC_CA_quad_10000000000001111(self):
        ssd = SevenSegmentConverterCAQuad(
            1, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 1, 1, 1, 1)
        assert ssd.get_output() == (
            1, 0, 0, 0, 0, 0, 0, 1, 0, 0, 0, 0, 0, 0,
            1, 0, 0, 0, 0, 0, 0, 0, 0, 0, 1, 1, 1, 0)

    def test_CONV_SSC_CA_quad_10000000011110000(self):
        ssd = SevenSegmentConverterCAQuad(
            1, 0, 0, 0, 0, 0, 0, 0, 0, 1, 1, 1, 1, 0, 0, 0, 0)
        assert ssd.get_output() == (
            1, 0, 0, 0, 0, 0, 0, 1, 0, 0, 0, 0, 0, 0,
            0, 0, 0, 1, 1, 1, 0, 1, 0, 0, 0, 0, 0, 0)

    def test_CONV_SSC_CA_quad_10000111100000000(self):
        ssd = SevenSegmentConverterCAQuad(
            1, 0, 0, 0, 0, 1, 1, 1, 1, 0, 0, 0, 0, 0, 0, 0, 0)
        assert ssd.get_output() == (
            1, 0, 0, 0, 0, 0, 0, 0, 0, 0, 1, 1, 1, 0,
            1, 0, 0, 0, 0, 0, 0, 1, 0, 0, 0, 0, 0, 0)

    def test_CONV_SSC_CA_quad_11111000000000000(self):
        ssd = SevenSegmentConverterCAQuad(
            1, 1, 1, 1, 1, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0)
        assert ssd.get_output() == (
            0, 0, 0, 1, 1, 1, 0, 1, 0, 0, 0, 0, 0, 0,
            1, 0, 0, 0, 0, 0, 0, 1, 0, 0, 0, 0, 0, 0)

    def test_CONV_SSC_CA_quad_11111111111111111(self):
        ssd = SevenSegmentConverterCAQuad(
            1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1)
        assert ssd.get_output() == (
            0, 0, 0, 1, 1, 1, 0, 0, 0, 0, 1, 1, 1, 0,
            0, 0, 0, 1, 1, 1, 0, 0, 0, 0, 1, 1, 1, 0)
