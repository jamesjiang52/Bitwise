import sys
sys.path.insert(0, "../../")
import signal
from MUX import Multiplexer4To1


class TestMultiplexer4To1:
    def test_MUX_4to1_0000000(self):
        mux = Multiplexer4To1(0, 0, 0, 0, 0, 0, 0)
        assert mux.get_output() == 0

    def test_MUX_4to1_0111111(self):
        mux = Multiplexer4To1(0, 1, 1, 1, 1, 1, 1)
        assert mux.get_output() == 0

    def test_MUX_4to1_1000000(self):
        mux = Multiplexer4To1(1, 0, 0, 0, 0, 0, 0)
        assert mux.get_output() == 0

    def test_MUX_4to1_1000001(self):
        mux = Multiplexer4To1(1, 0, 0, 0, 0, 0, 1)
        assert mux.get_output() == 1

    def test_MUX_4to1_1001110(self):
        mux = Multiplexer4To1(1, 0, 0, 1, 1, 1, 0)
        assert mux.get_output() == 0

    def test_MUX_4to1_1001111(self):
        mux = Multiplexer4To1(1, 0, 0, 1, 1, 1, 1)
        assert mux.get_output() == 1

    def test_MUX_4to1_1010000(self):
        mux = Multiplexer4To1(1, 0, 1, 0, 0, 0, 0)
        assert mux.get_output() == 0

    def test_MUX_4to1_1010010(self):
        mux = Multiplexer4To1(1, 0, 1, 0, 0, 1, 0)
        assert mux.get_output() == 1

    def test_MUX_4to1_1011101(self):
        mux = Multiplexer4To1(1, 0, 1, 1, 1, 0, 1)
        assert mux.get_output() == 0

    def test_MUX_4to1_1011111(self):
        mux = Multiplexer4To1(1, 0, 1, 1, 1, 1, 1)
        assert mux.get_output() == 1

    def test_MUX_4to1_1100000(self):
        mux = Multiplexer4To1(1, 1, 0, 0, 0, 0, 0)
        assert mux.get_output() == 0

    def test_MUX_4to1_1100100(self):
        mux = Multiplexer4To1(1, 1, 0, 0, 1, 0, 0)
        assert mux.get_output() == 1

    def test_MUX_4to1_1101011(self):
        mux = Multiplexer4To1(1, 1, 0, 1, 0, 1, 1)
        assert mux.get_output() == 0

    def test_MUX_4to1_1101111(self):
        mux = Multiplexer4To1(1, 1, 0, 1, 1, 1, 1)
        assert mux.get_output() == 1

    def test_MUX_4to1_1110000(self):
        mux = Multiplexer4To1(1, 1, 1, 0, 0, 0, 0)
        assert mux.get_output() == 0

    def test_MUX_4to1_1111000(self):
        mux = Multiplexer4To1(1, 1, 1, 1, 0, 0, 0)
        assert mux.get_output() == 1

    def test_MUX_4to1_1110111(self):
        mux = Multiplexer4To1(1, 1, 1, 0, 1, 1, 1)
        assert mux.get_output() == 0

    def test_MUX_4to1_1111111(self):
        mux = Multiplexer4To1(1, 1, 1, 1, 1, 1, 1)
        assert mux.get_output() == 1
