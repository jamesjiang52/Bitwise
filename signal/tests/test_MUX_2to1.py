import sys
sys.path.append("..")
from MUX import MUX2To1


class TestMUX2To1:
    def test_MUX_2to1_0000(self):
        mux = MUX2To1(1, 0, 0, 0)
        assert mux.get_output() == 0

    def test_MUX_2to1_0111(self):
        mux = MUX2To1(0, 1, 1, 1)
        assert mux.get_output() == 0

    def test_MUX_2to1_1000(self):
        mux = MUX2To1(1, 0, 0, 0)
        assert mux.get_output() == 0

    def test_MUX_2to1_1001(self):
        mux = MUX2To1(1, 0, 0, 1)
        assert mux.get_output() == 1

    def test_MUX_2to1_1010(self):
        mux = MUX2To1(1, 0, 1, 0)
        assert mux.get_output() == 0

    def test_MUX_2to1_1011(self):
        mux = MUX2To1(1, 0, 1, 1)
        assert mux.get_output() == 1

    def test_MUX_2to1_1100(self):
        mux = MUX2To1(1, 1, 0, 0)
        assert mux.get_output() == 0

    def test_MUX_2to1_1101(self):
        mux = MUX2To1(1, 1, 0, 1)
        assert mux.get_output() == 0

    def test_MUX_2to1_1110(self):
        mux = MUX2To1(1, 1, 1, 0)
        assert mux.get_output() == 1

    def test_MUX_2to1_1111(self):
        mux = MUX2To1(1, 1, 1, 1)
        assert mux.get_output() == 1
