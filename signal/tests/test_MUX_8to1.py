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

    def test_MUX_8to1_100001111111(self):
        mux = MUX8To1(1, 0, 0, 0, 0, 1, 1, 1, 1, 1, 1, 1)
        assert mux.get_output() == 0

    def test_MUX_8to1_100010000000(self):
        mux = MUX8To1(1, 0, 0, 0, 1, 0, 0, 0, 0, 0, 0, 0)
        assert mux.get_output() == 1

    def test_MUX_8to1_100011111111(self):
        mux = MUX8To1(1, 0, 0, 0, 1, 1, 1, 1, 1, 1, 1, 1)
        assert mux.get_output() == 1

    def test_MUX_8to1_100100000000(self):
        mux = MUX8To1(1, 0, 0, 1, 0, 0, 0, 0, 0, 0, 0, 0)
        assert mux.get_output() == 0

    def test_MUX_8to1_100110111111(self):
        mux = MUX8To1(1, 0, 0, 1, 1, 0, 1, 1, 1, 1, 1, 1)
        assert mux.get_output() == 0

    def test_MUX_8to1_100101000000(self):
        mux = MUX8To1(1, 0, 0, 1, 0, 1, 0, 0, 0, 0, 0, 0)
        assert mux.get_output() == 1

    def test_MUX_8to1_100111111111(self):
        mux = MUX8To1(1, 0, 0, 1, 1, 1, 1, 1, 1, 1, 1, 1)
        assert mux.get_output() == 1

    def test_MUX_8to1_101000000000(self):
        mux = MUX8To1(1, 0, 1, 0, 0, 0, 0, 0, 0, 0, 0, 0)
        assert mux.get_output() == 0

    def test_MUX_8to1_101011011111(self):
        mux = MUX8To1(1, 0, 1, 0, 1, 1, 0, 1, 1, 1, 1, 1)
        assert mux.get_output() == 0

    def test_MUX_8to1_101000100000(self):
        mux = MUX8To1(1, 0, 1, 0, 0, 0, 1, 0, 0, 0, 0, 0)
        assert mux.get_output() == 1

    def test_MUX_8to1_101011111111(self):
        mux = MUX8To1(1, 0, 1, 0, 1, 1, 1, 1, 1, 1, 1, 1)
        assert mux.get_output() == 1

    def test_MUX_8to1_101100000000(self):
        mux = MUX8To1(1, 0, 1, 1, 0, 0, 0, 0, 0, 0, 0, 0)
        assert mux.get_output() == 0

    def test_MUX_8to1_101111101111(self):
        mux = MUX8To1(1, 0, 1, 1, 1, 1, 1, 0, 1, 1, 1, 1)
        assert mux.get_output() == 0

    def test_MUX_8to1_101100010000(self):
        mux = MUX8To1(1, 0, 1, 1, 0, 0, 0, 1, 0, 0, 0, 0)
        assert mux.get_output() == 1

    def test_MUX_8to1_101111111111(self):
        mux = MUX8To1(1, 0, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1)
        assert mux.get_output() == 1

    def test_MUX_8to1_110000000000(self):
        mux = MUX8To1(1, 1, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0)
        assert mux.get_output() == 0

    def test_MUX_8to1_110001110111(self):
        mux = MUX8To1(1, 1, 0, 0, 0, 1, 1, 1, 0, 1, 1, 1)
        assert mux.get_output() == 0

    def test_MUX_8to1_110000001000(self):
        mux = MUX8To1(1, 1, 0, 0, 0, 0, 0, 0, 1, 0, 0, 0)
        assert mux.get_output() == 1

    def test_MUX_8to1_110011111111(self):
        mux = MUX8To1(1, 1, 0, 0, 1, 1, 1, 1, 1, 1, 1, 1)
        assert mux.get_output() == 1

    def test_MUX_8to1_110100000000(self):
        mux = MUX8To1(1, 1, 0, 1, 0, 0, 0, 0, 0, 0, 0, 0)
        assert mux.get_output() == 0

    def test_MUX_8to1_110111111011(self):
        mux = MUX8To1(1, 1, 0, 1, 1, 1, 1, 1, 1, 0, 1, 1)
        assert mux.get_output() == 0

    def test_MUX_8to1_110100000100(self):
        mux = MUX8To1(1, 1, 0, 1, 0, 0, 0, 0, 0, 1, 0, 0)
        assert mux.get_output() == 1

    def test_MUX_8to1_110111111111(self):
        mux = MUX8To1(1, 1, 0, 1, 1, 1, 1, 1, 1, 1, 1, 1)
        assert mux.get_output() == 1

    def test_MUX_8to1_111000000000(self):
        mux = MUX8To1(1, 1, 1, 0, 0, 0, 0, 0, 0, 0, 0, 0)
        assert mux.get_output() == 0

    def test_MUX_8to1_111011111101(self):
        mux = MUX8To1(1, 1, 1, 0, 1, 1, 1, 1, 1, 1, 0, 1)
        assert mux.get_output() == 0

    def test_MUX_8to1_111000000010(self):
        mux = MUX8To1(1, 1, 1, 0, 0, 0, 0, 0, 0, 0, 1, 0)
        assert mux.get_output() == 1

    def test_MUX_8to1_111011111111(self):
        mux = MUX8To1(1, 1, 1, 0, 1, 1, 1, 1, 1, 1, 1, 1)
        assert mux.get_output() == 1

    def test_MUX_8to1_111100000000(self):
        mux = MUX8To1(1, 1, 1, 1, 0, 0, 0, 0, 0, 0, 0, 0)
        assert mux.get_output() == 0

    def test_MUX_8to1_111111111110(self):
        mux = MUX8To1(1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 0)
        assert mux.get_output() == 0

    def test_MUX_8to1_111100000001(self):
        mux = MUX8To1(1, 1, 1, 1, 0, 0, 0, 0, 0, 0, 0, 1)
        assert mux.get_output() == 1

    def test_MUX_8to1_111111111111(self):
        mux = MUX8To1(1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1)
        assert mux.get_output() == 1
