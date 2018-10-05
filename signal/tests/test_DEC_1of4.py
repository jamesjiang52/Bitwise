import sys
sys.path.insert(0, "../../")
import signal
from DEC import Decoder1Of4


class TestDecoder1Of4:
    def test_DEC_1of4_000(self):
        dec = Decoder1Of4(0, 0, 0)
        assert dec.get_output() == (0, 0, 0, 0)

    def test_DEC_1of4_001(self):
        dec = Decoder1Of4(0, 0, 1)
        assert dec.get_output() == (0, 0, 0, 0)

    def test_DEC_1of4_010(self):
        dec = Decoder1Of4(0, 1, 0)
        assert dec.get_output() == (0, 0, 0, 0)

    def test_DEC_1of4_011(self):
        dec = Decoder1Of4(0, 1, 1)
        assert dec.get_output() == (0, 0, 0, 0)

    def test_DEC_1of4_100(self):
        dec = Decoder1Of4(1, 0, 0)
        assert dec.get_output() == (0, 0, 0, 1)

    def test_DEC_1of4_101(self):
        dec = Decoder1Of4(1, 0, 1)
        assert dec.get_output() == (0, 0, 1, 0)

    def test_DEC_1of4_110(self):
        dec = Decoder1Of4(1, 1, 0)
        assert dec.get_output() == (0, 1, 0, 0)

    def test_DEC_1of4_111(self):
        dec = Decoder1Of4(1, 1, 1)
        assert dec.get_output() == (1, 0, 0, 0)
