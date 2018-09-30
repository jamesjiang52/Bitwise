import sys
sys.path.insert(0, "../../")
import signal
from DEC import DEC1Of8


class TestDEC1Of8:
    def test_DEC_1of8_0000(self):
        dec = DEC1Of8(0, 0, 0, 0)
        assert dec.get_output() == (0, 0, 0, 0, 0, 0, 0, 0)

    def test_DEC_1of8_0111(self):
        dec = DEC1Of8(0, 1, 1, 1)
        assert dec.get_output() == (0, 0, 0, 0, 0, 0, 0, 0)

    def test_DEC_1of8_1000(self):
        dec = DEC1Of8(1, 0, 0, 0)
        assert dec.get_output() == (0, 0, 0, 0, 0, 0, 0, 1)

    def test_DEC_1of8_1001(self):
        dec = DEC1Of8(1, 0, 0, 1)
        assert dec.get_output() == (0, 0, 0, 0, 0, 0, 1, 0)

    def test_DEC_1of8_1010(self):
        dec = DEC1Of8(1, 0, 1, 0)
        assert dec.get_output() == (0, 0, 0, 0, 0, 1, 0, 0)

    def test_DEC_1of8_1011(self):
        dec = DEC1Of8(1, 0, 1, 1)
        assert dec.get_output() == (0, 0, 0, 0, 1, 0, 0, 0)

    def test_DEC_1of8_1100(self):
        dec = DEC1Of8(1, 1, 0, 0)
        assert dec.get_output() == (0, 0, 0, 1, 0, 0, 0, 0)

    def test_DEC_1of8_1101(self):
        dec = DEC1Of8(1, 1, 0, 1)
        assert dec.get_output() == (0, 0, 1, 0, 0, 0, 0, 0)

    def test_DEC_1of8_1110(self):
        dec = DEC1Of8(1, 1, 1, 0)
        assert dec.get_output() == (0, 1, 0, 0, 0, 0, 0, 0)

    def test_DEC_1of8_1111(self):
        dec = DEC1Of8(1, 1, 1, 1)
        assert dec.get_output() == (1, 0, 0, 0, 0, 0, 0, 0)
