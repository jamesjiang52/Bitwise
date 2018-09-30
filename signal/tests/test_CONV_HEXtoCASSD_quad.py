import sys
sys.path.insert(0, "../../")
import signal
from CONV import CONVHEXToCASSDQuad


class TestCONVHEXToCASSDQuad:
    def test_CONV_HEXtoCASSD_quad_00000000000000000(self):
        ssd = CONVHEXToCASSDQuad(
            0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0)
        assert ssd.get_output() == (
            1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1,
            1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1)

    def test_CONV_HEXtoCASSD_quad_01111111111111111(self):
        ssd = CONVHEXToCASSDQuad(
            0, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1)
        assert ssd.get_output() == (
            1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1,
            1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1)

    def test_CONV_HEXtoCASSD_quad_10000000000000000(self):
        ssd = CONVHEXToCASSDQuad(
            1, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0)
        assert ssd.get_output() == (
            1, 0, 0, 0, 0, 0, 0, 1, 0, 0, 0, 0, 0, 0,
            1, 0, 0, 0, 0, 0, 0, 1, 0, 0, 0, 0, 0, 0)

    def test_CONV_HEXtoCASSD_quad_10000000000001111(self):
        ssd = CONVHEXToCASSDQuad(
            1, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 1, 1, 1, 1)
        assert ssd.get_output() == (
            1, 0, 0, 0, 0, 0, 0, 1, 0, 0, 0, 0, 0, 0,
            1, 0, 0, 0, 0, 0, 0, 0, 0, 0, 1, 1, 1, 0)

    def test_CONV_HEXtoCASSD_quad_10000000011110000(self):
        ssd = CONVHEXToCASSDQuad(
            1, 0, 0, 0, 0, 0, 0, 0, 0, 1, 1, 1, 1, 0, 0, 0, 0)
        assert ssd.get_output() == (
            1, 0, 0, 0, 0, 0, 0, 1, 0, 0, 0, 0, 0, 0,
            0, 0, 0, 1, 1, 1, 0, 1, 0, 0, 0, 0, 0, 0)

    def test_CONV_HEXtoCASSD_quad_10000111100000000(self):
        ssd = CONVHEXToCASSDQuad(
            1, 0, 0, 0, 0, 1, 1, 1, 1, 0, 0, 0, 0, 0, 0, 0, 0)
        assert ssd.get_output() == (
            1, 0, 0, 0, 0, 0, 0, 0, 0, 0, 1, 1, 1, 0,
            1, 0, 0, 0, 0, 0, 0, 1, 0, 0, 0, 0, 0, 0)

    def test_CONV_HEXtoCASSD_quad_11111000000000000(self):
        ssd = CONVHEXToCASSDQuad(
            1, 1, 1, 1, 1, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0)
        assert ssd.get_output() == (
            0, 0, 0, 1, 1, 1, 0, 1, 0, 0, 0, 0, 0, 0,
            1, 0, 0, 0, 0, 0, 0, 1, 0, 0, 0, 0, 0, 0)

    def test_CONV_HEXtoCASSD_quad_11111111111111111(self):
        ssd = CONVHEXToCASSDQuad(
            1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1)
        assert ssd.get_output() == (
            0, 0, 0, 1, 1, 1, 0, 0, 0, 0, 1, 1, 1, 0,
            0, 0, 0, 1, 1, 1, 0, 0, 0, 0, 1, 1, 1, 0)
