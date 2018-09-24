import sys
sys.path.append("..")
from MUX import MUX8To1


class TestMUX8To1:
    def test_MUX_8to1_000000000000(self):
        mux = MUX8To1(0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0)
        assert mux.get_output() == 0

    def test_MUX_8to1_011111111111(self):
        mux = MUX8To1(0, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1)
        assert mux.get_output() == 0

    def test_MUX_8to1_100000000000(self):
        mux = MUX8To1(1, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0)
        assert mux.get_output() == 0

    def test_MUX_8to1_100000000001(self):
        mux = MUX8To1(1, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 1)
        assert mux.get_output() == 1

    def test_MUX_8to1_100011111110(self):
        mux = MUX8To1(1, 0, 0, 0, 1, 1, 1, 1, 1, 1, 1, 0)
        assert mux.get_output() == 0

    def test_MUX_8to1_100011111111(self):
        mux = MUX8To1(1, 0, 0, 0, 1, 1, 1, 1, 1, 1, 1, 1)
        assert mux.get_output() == 1

    def test_MUX_8to1_100100000000(self):
        mux = MUX8To1(1, 0, 0, 1, 0, 0, 0, 0, 0, 0, 0, 0)
        assert mux.get_output() == 0

    def test_MUX_8to1_100100000010(self):
        mux = MUX8To1(1, 0, 0, 1, 0, 0, 0, 0, 0, 0, 1, 0)
        assert mux.get_output() == 1

    def test_MUX_8to1_100111111101(self):
        mux = MUX8To1(1, 0, 0, 1, 1, 1, 1, 1, 1, 1, 0, 1)
        assert mux.get_output() == 0

    def test_MUX_8to1_100111111111(self):
        mux = MUX8To1(1, 0, 0, 1, 1, 1, 1, 1, 1, 1, 1, 1)
        assert mux.get_output() == 1

    def test_MUX_8to1_101000000000(self):
        mux = MUX8To1(1, 0, 1, 0, 0, 0, 0, 0, 0, 0, 0, 0)
        assert mux.get_output() == 0

    def test_MUX_8to1_101000000100(self):
        mux = MUX8To1(1, 0, 1, 0, 0, 0, 0, 0, 0, 1, 0, 0)
        assert mux.get_output() == 1

    def test_MUX_8to1_101011111011(self):
        mux = MUX8To1(1, 0, 1, 0, 1, 1, 1, 1, 1, 0, 1, 1)
        assert mux.get_output() == 0

    def test_MUX_8to1_101011111111(self):
        mux = MUX8To1(1, 0, 1, 0, 1, 1, 1, 1, 1, 1, 1, 1)
        assert mux.get_output() == 1

    def test_MUX_8to1_101100000000(self):
        mux = MUX8To1(1, 0, 1, 1, 0, 0, 0, 0, 0, 0, 0, 0)
        assert mux.get_output() == 0

    def test_MUX_8to1_101100001000(self):
        mux = MUX8To1(1, 0, 1, 1, 0, 0, 0, 0, 1, 0, 0, 0)
        assert mux.get_output() == 1

    def test_MUX_8to1_101111110111(self):
        mux = MUX8To1(1, 0, 1, 1, 1, 1, 1, 1, 0, 1, 1, 1)
        assert mux.get_output() == 0

    def test_MUX_8to1_101111111111(self):
        mux = MUX8To1(1, 0, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1)
        assert mux.get_output() == 1

    def test_MUX_8to1_110000000000(self):
        mux = MUX8To1(1, 1, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0)
        assert mux.get_output() == 0

    def test_MUX_8to1_110000010000(self):
        mux = MUX8To1(1, 1, 0, 0, 0, 0, 0, 1, 0, 0, 0, 0)
        assert mux.get_output() == 1

    def test_MUX_8to1_110011101111(self):
        mux = MUX8To1(1, 1, 0, 0, 1, 1, 1, 0, 1, 1, 1, 1)
        assert mux.get_output() == 0

    def test_MUX_8to1_110011111111(self):
        mux = MUX8To1(1, 1, 0, 0, 1, 1, 1, 1, 1, 1, 1, 1)
        assert mux.get_output() == 1

    def test_MUX_8to1_110100000000(self):
        mux = MUX8To1(1, 1, 0, 1, 0, 0, 0, 0, 0, 0, 0, 0)
        assert mux.get_output() == 0

    def test_MUX_8to1_110100100000(self):
        mux = MUX8To1(1, 1, 0, 1, 0, 0, 1, 0, 0, 0, 0, 0)
        assert mux.get_output() == 1

    def test_MUX_8to1_110111011111(self):
        mux = MUX8To1(1, 1, 0, 1, 1, 1, 0, 1, 1, 1, 1, 1)
        assert mux.get_output() == 0

    def test_MUX_8to1_110111111111(self):
        mux = MUX8To1(1, 1, 0, 1, 1, 1, 1, 1, 1, 1, 1, 1)
        assert mux.get_output() == 1

    def test_MUX_8to1_111000000000(self):
        mux = MUX8To1(1, 1, 1, 0, 0, 0, 0, 0, 0, 0, 0, 0)
        assert mux.get_output() == 0

    def test_MUX_8to1_111001000000(self):
        mux = MUX8To1(1, 1, 1, 0, 0, 1, 0, 0, 0, 0, 0, 0)
        assert mux.get_output() == 1

    def test_MUX_8to1_111010111111(self):
        mux = MUX8To1(1, 1, 1, 0, 1, 0, 1, 1, 1, 1, 1, 1)
        assert mux.get_output() == 0

    def test_MUX_8to1_111011111111(self):
        mux = MUX8To1(1, 1, 1, 0, 1, 1, 1, 1, 1, 1, 1, 1)
        assert mux.get_output() == 1

    def test_MUX_8to1_111100000000(self):
        mux = MUX8To1(1, 1, 1, 1, 0, 0, 0, 0, 0, 0, 0, 0)
        assert mux.get_output() == 0

    def test_MUX_8to1_111110000000(self):
        mux = MUX8To1(1, 1, 1, 1, 1, 0, 0, 0, 0, 0, 0, 0)
        assert mux.get_output() == 1

    def test_MUX_8to1_111101111111(self):
        mux = MUX8To1(1, 1, 1, 1, 0, 1, 1, 1, 1, 1, 1, 1)
        assert mux.get_output() == 0

    def test_MUX_8to1_111111111111(self):
        mux = MUX8To1(1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1)
        assert mux.get_output() == 1
