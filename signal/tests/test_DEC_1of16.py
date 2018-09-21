import sys
sys.path.append("..")
from DEC import DEC1Of16


class TestDEC1Of16:
    def test_DEC_1of16_00000(self):
        dec = DEC1Of16(0, 0, 0, 0, 0)
        assert dec.get_output() == (
            0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0)

    def test_DEC_1of16_01111(self):
        dec = DEC1Of16(0, 1, 1, 1, 1)
        assert dec.get_output() == (
            0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0)

    def test_DEC_1of16_10000(self):
        dec = DEC1Of16(1, 0, 0, 0, 0)
        assert dec.get_output() == (
            0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 1)

    def test_DEC_1of16_10001(self):
        dec = DEC1Of16(1, 0, 0, 0, 1)
        assert dec.get_output() == (
            0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 1, 0)

    def test_DEC_1of16_10010(self):
        dec = DEC1Of16(1, 0, 0, 1, 0)
        assert dec.get_output() == (
            0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 1, 0, 0)

    def test_DEC_1of16_10011(self):
        dec = DEC1Of16(1, 0, 0, 1, 1)
        assert dec.get_output() == (
            0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 1, 0, 0, 0)

    def test_DEC_1of16_10100(self):
        dec = DEC1Of16(1, 0, 1, 0, 0)
        assert dec.get_output() == (
            0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 1, 0, 0, 0, 0)

    def test_DEC_1of16_10101(self):
        dec = DEC1Of16(1, 0, 1, 0, 1)
        assert dec.get_output() == (
            0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 1, 0, 0, 0, 0, 0)

    def test_DEC_1of16_10110(self):
        dec = DEC1Of16(1, 0, 1, 1, 0)
        assert dec.get_output() == (
            0, 0, 0, 0, 0, 0, 0, 0, 0, 1, 0, 0, 0, 0, 0, 0)

    def test_DEC_1of16_10111(self):
        dec = DEC1Of16(1, 0, 1, 1, 1)
        assert dec.get_output() == (
            0, 0, 0, 0, 0, 0, 0, 0, 1, 0, 0, 0, 0, 0, 0, 0)

    def test_DEC_1of16_11000(self):
        dec = DEC1Of16(1, 1, 0, 0, 0)
        assert dec.get_output() == (
            0, 0, 0, 0, 0, 0, 0, 1, 0, 0, 0, 0, 0, 0, 0, 0)

    def test_DEC_1of16_11001(self):
        dec = DEC1Of16(1, 1, 0, 0, 1)
        assert dec.get_output() == (
            0, 0, 0, 0, 0, 0, 1, 0, 0, 0, 0, 0, 0, 0, 0, 0)

    def test_DEC_1of16_11010(self):
        dec = DEC1Of16(1, 1, 0, 1, 0)
        assert dec.get_output() == (
            0, 0, 0, 0, 0, 1, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0)

    def test_DEC_1of16_11011(self):
        dec = DEC1Of16(1, 1, 0, 1, 1)
        assert dec.get_output() == (
            0, 0, 0, 0, 1, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0)

    def test_DEC_1of16_11100(self):
        dec = DEC1Of16(1, 1, 1, 0, 0)
        assert dec.get_output() == (
            0, 0, 0, 1, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0)

    def test_DEC_1of16_11101(self):
        dec = DEC1Of16(1, 1, 1, 0, 1)
        assert dec.get_output() == (
            0, 0, 1, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0)

    def test_DEC_1of16_11110(self):
        dec = DEC1Of16(1, 1, 1, 1, 0)
        assert dec.get_output() == (
            0, 1, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0)

    def test_DEC_1of16_11111(self):
        dec = DEC1Of16(1, 1, 1, 1, 1)
        assert dec.get_output() == (
            1, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0)
