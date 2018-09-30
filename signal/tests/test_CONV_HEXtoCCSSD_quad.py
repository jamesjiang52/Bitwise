import sys
sys.path.insert(0, "../../")
import signal
from CONV import CONVHEXToCCSSDQuad


class TestCONVHEXToCCSSDQuad:
    def test_CONV_HEXtoCCSSD_quad_00000000000000000(self):
        ssd = CONVHEXToCCSSDQuad(
            0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0)
        assert ssd.get_output() == (
            0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,
            0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0)

    def test_CONV_HEXtoCCSSD_quad_01111111111111111(self):
        ssd = CONVHEXToCCSSDQuad(
            0, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1)
        assert ssd.get_output() == (
            0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,
            0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0)

    def test_CONV_HEXtoCCSSD_quad_10000000000000000(self):
        ssd = CONVHEXToCCSSDQuad(
            1, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0)
        assert ssd.get_output() == (
            0, 1, 1, 1, 1, 1, 1, 0, 1, 1, 1, 1, 1, 1,
            0, 1, 1, 1, 1, 1, 1, 0, 1, 1, 1, 1, 1, 1)

    def test_CONV_HEXtoCCSSD_quad_10000000000001111(self):
        ssd = CONVHEXToCCSSDQuad(
            1, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 1, 1, 1, 1)
        assert ssd.get_output() == (
            0, 1, 1, 1, 1, 1, 1, 0, 1, 1, 1, 1, 1, 1,
            0, 1, 1, 1, 1, 1, 1, 1, 1, 1, 0, 0, 0, 1)

    def test_CONV_HEXtoCCSSD_quad_10000000011110000(self):
        ssd = CONVHEXToCCSSDQuad(
            1, 0, 0, 0, 0, 0, 0, 0, 0, 1, 1, 1, 1, 0, 0, 0, 0)
        assert ssd.get_output() == (
            0, 1, 1, 1, 1, 1, 1, 0, 1, 1, 1, 1, 1, 1,
            1, 1, 1, 0, 0, 0, 1, 0, 1, 1, 1, 1, 1, 1)

    def test_CONV_HEXtoCCSSD_quad_10000111100000000(self):
        ssd = CONVHEXToCCSSDQuad(
            1, 0, 0, 0, 0, 1, 1, 1, 1, 0, 0, 0, 0, 0, 0, 0, 0)
        assert ssd.get_output() == (
            0, 1, 1, 1, 1, 1, 1, 1, 1, 1, 0, 0, 0, 1,
            0, 1, 1, 1, 1, 1, 1, 0, 1, 1, 1, 1, 1, 1)

    def test_CONV_HEXtoCCSSD_quad_11111000000000000(self):
        ssd = CONVHEXToCCSSDQuad(
            1, 1, 1, 1, 1, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0)
        assert ssd.get_output() == (
            1, 1, 1, 0, 0, 0, 1, 0, 1, 1, 1, 1, 1, 1,
            0, 1, 1, 1, 1, 1, 1, 0, 1, 1, 1, 1, 1, 1)

    def test_CONV_HEXtoCCSSD_quad_11111111111111111(self):
        ssd = CONVHEXToCCSSDQuad(
            1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1)
        assert ssd.get_output() == (
            1, 1, 1, 0, 0, 0, 1, 1, 1, 1, 0, 0, 0, 1,
            1, 1, 1, 0, 0, 0, 1, 1, 1, 1, 0, 0, 0, 1)
