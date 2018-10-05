import sys
sys.path.insert(0, "../../")
import signal
from MUX import Multiplexer2To1


class TestMultiplexer2To1:
    def test_MUX_2to1_0000(self):
        mux = Multiplexer2To1(1, 0, 0, 0)
        assert mux.get_output() == 0

    def test_MUX_2to1_0111(self):
        mux = Multiplexer2To1(0, 1, 1, 1)
        assert mux.get_output() == 0

    def test_MUX_2to1_1000(self):
        mux = Multiplexer2To1(1, 0, 0, 0)
        assert mux.get_output() == 0

    def test_MUX_2to1_1001(self):
        mux = Multiplexer2To1(1, 0, 0, 1)
        assert mux.get_output() == 1

    def test_MUX_2to1_1010(self):
        mux = Multiplexer2To1(1, 0, 1, 0)
        assert mux.get_output() == 0

    def test_MUX_2to1_1011(self):
        mux = Multiplexer2To1(1, 0, 1, 1)
        assert mux.get_output() == 1

    def test_MUX_2to1_1100(self):
        mux = Multiplexer2To1(1, 1, 0, 0)
        assert mux.get_output() == 0

    def test_MUX_2to1_1101(self):
        mux = Multiplexer2To1(1, 1, 0, 1)
        assert mux.get_output() == 0

    def test_MUX_2to1_1110(self):
        mux = Multiplexer2To1(1, 1, 1, 0)
        assert mux.get_output() == 1

    def test_MUX_2to1_1111(self):
        mux = Multiplexer2To1(1, 1, 1, 1)
        assert mux.get_output() == 1
