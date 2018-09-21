import sys
sys.path.append("..")
from MUX import MUX4To1


class TestMUX4To1:
    def test_MUX_4to1_0000000(self):
        mux = MUX4To1(0, 0, 0, 0, 0, 0, 0)
        assert mux.get_output() == 0

    def test_MUX_4to1_0111111(self):
        mux = MUX4To1(0, 1, 1, 1, 1, 1, 1)
        assert mux.get_output() == 0

    def test_MUX_4to1_1000000(self):
        mux = MUX4To1(1, 0, 0, 0, 0, 0, 0)
        assert mux.get_output() == 0

    def test_MUX_4to1_1000111(self):
        mux = MUX4To1(1, 0, 0, 0, 1, 1, 1)
        assert mux.get_output() == 0

    def test_MUX_4to1_1001000(self):
        mux = MUX4To1(1, 0, 0, 1, 0, 0, 0)
        assert mux.get_output() == 1

    def test_MUX_4to1_1001111(self):
        mux = MUX4To1(1, 0, 0, 1, 1, 1, 1)
        assert mux.get_output() == 1

    def test_MUX_4to1_1010000(self):
        mux = MUX4To1(1, 0, 1, 0, 0, 0, 0)
        assert mux.get_output() == 0

    def test_MUX_4to1_1011011(self):
        mux = MUX4To1(1, 0, 1, 1, 0, 1, 1)
        assert mux.get_output() == 0

    def test_MUX_4to1_1010100(self):
        mux = MUX4To1(1, 0, 1, 0, 1, 0, 0)
        assert mux.get_output() == 1

    def test_MUX_4to1_1011111(self):
        mux = MUX4To1(1, 0, 1, 1, 1, 1, 1)
        assert mux.get_output() == 1

    def test_MUX_4to1_110000(self):
        mux = MUX4To1(1, 1, 0, 0, 0, 0, 0)
        assert mux.get_output() == 0

    def test_MUX_4to1_1101101(self):
        mux = MUX4To1(1, 1, 0, 1, 1, 0, 1)
        assert mux.get_output() == 0

    def test_MUX_4to1_1100010(self):
        mux = MUX4To1(1, 1, 0, 0, 0, 1, 0)
        assert mux.get_output() == 1

    def test_MUX_4to1_1101111(self):
        mux = MUX4To1(1, 1, 0, 1, 1, 1, 1)
        assert mux.get_output() == 1

    def test_MUX_4to1_1110000(self):
        mux = MUX4To1(1, 1, 1, 0, 0, 0, 0)
        assert mux.get_output() == 0

    def test_MUX_4to1_1111110(self):
        mux = MUX4To1(1, 1, 1, 1, 1, 1, 0)
        assert mux.get_output() == 0

    def test_MUX_4to1_1110001(self):
        mux = MUX4To1(1, 1, 1, 0, 0, 0, 1)
        assert mux.get_output() == 1

    def test_MUX_4to1_1111111(self):
        mux = MUX4To1(1, 1, 1, 1, 1, 1, 1)
        assert mux.get_output() == 1
