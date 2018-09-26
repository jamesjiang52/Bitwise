import sys
sys.path.append("..")
from CONV import CONVHEXToCSSSDQuad


class TestCONVHEXToCSSSDQuad:
    def test_CONV_HEXtoCSSSD_quad_00000000000000000(self):
        ssd = CONVHEXToCSSSDQuad(
            0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0)
        assert ssd.get_output() == (
            0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,
            0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0)

    def test_CONV_HEXtoCSSSD_quad_01111111111111111(self):
        ssd = CONVHEXToCSSSDQuad(
            0, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1)
        assert ssd.get_output() == (
            0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,
            0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0)

    def test_CONV_HEXtoCSSSD_quad_10000000000000000(self):
        ssd = CONVHEXToCSSSDQuad(
            1, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0)
        assert ssd.get_output() == (
            0, 1, 1, 1, 1, 1, 1, 0, 1, 1, 1, 1, 1, 1,
            0, 1, 1, 1, 1, 1, 1, 0, 1, 1, 1, 1, 1, 1)

    def test_CONV_HEXtoCSSSD_quad_10000000000001111(self):
        ssd = CONVHEXToCSSSDQuad(
            1, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 1, 1, 1, 1)
        assert ssd.get_output() == (
            0, 1, 1, 1, 1, 1, 1, 0, 1, 1, 1, 1, 1, 1,
            0, 1, 1, 1, 1, 1, 1, 1, 1, 1, 0, 0, 0, 1)

    def test_CONV_HEXtoCSSSD_quad_10000000011110000(self):
        ssd = CONVHEXToCSSSDQuad(
            1, 0, 0, 0, 0, 0, 0, 0, 0, 1, 1, 1, 1, 0, 0, 0, 0)
        assert ssd.get_output() == (
            0, 1, 1, 1, 1, 1, 1, 0, 1, 1, 1, 1, 1, 1,
            1, 1, 1, 0, 0, 0, 1, 0, 1, 1, 1, 1, 1, 1)

    def test_CONV_HEXtoCSSSD_quad_10000111100000000(self):
        ssd = CONVHEXToCSSSDQuad(
            1, 0, 0, 0, 0, 1, 1, 1, 1, 0, 0, 0, 0, 0, 0, 0, 0)
        assert ssd.get_output() == (
            0, 1, 1, 1, 1, 1, 1, 1, 1, 1, 0, 0, 0, 1,
            0, 1, 1, 1, 1, 1, 1, 0, 1, 1, 1, 1, 1, 1)

    def test_CONV_HEXtoCSSSD_quad_11111000000000000(self):
        ssd = CONVHEXToCSSSDQuad(
            1, 1, 1, 1, 1, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0)
        assert ssd.get_output() == (
            1, 1, 1, 0, 0, 0, 1, 0, 1, 1, 1, 1, 1, 1,
            0, 1, 1, 1, 1, 1, 1, 0, 1, 1, 1, 1, 1, 1)

    def test_CONV_HEXtoCSSSD_quad_11111111111111111(self):
        ssd = CONVHEXToCSSSDQuad(
            1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1)
        assert ssd.get_output() == (
            1, 1, 1, 0, 0, 0, 1, 1, 1, 1, 0, 0, 0, 1,
            1, 1, 1, 0, 0, 0, 1, 1, 1, 1, 0, 0, 0, 1)
