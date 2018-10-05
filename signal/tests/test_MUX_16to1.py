import sys
sys.path.insert(0, "../../")
import signal
from MUX import Multiplexer16To1


class TestMultiplexer16To1:
    def test_MUX_16to1_000000000000000000000(self):
        mux = Multiplexer16To1(
            0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0)
        assert mux.get_output() == 0

    def test_MUX_16to1_011111111111111111111(self):
        mux = Multiplexer16To1(
            0, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1)
        assert mux.get_output() == 0

    def test_MUX_16to1_100000000000000000000(self):
        mux = Multiplexer16To1(
            1, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0)
        assert mux.get_output() == 0

    def test_MUX_16to1_100000000000000000001(self):
        mux = Multiplexer16To1(
            1, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 1)
        assert mux.get_output() == 1

    def test_MUX_16to1_100001111111111111110(self):
        mux = Multiplexer16To1(
            1, 0, 0, 0, 0, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 0)
        assert mux.get_output() == 0

    def test_MUX_16to1_100001111111111111111(self):
        mux = Multiplexer16To1(
            1, 0, 0, 0, 0, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1)
        assert mux.get_output() == 1

    def test_MUX_16to1_100010000000000000000(self):
        mux = Multiplexer16To1(
            1, 0, 0, 0, 1, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0)
        assert mux.get_output() == 0

    def test_MUX_16to1_100010000000000000010(self):
        mux = Multiplexer16To1(
            1, 0, 0, 0, 1, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 1, 0)
        assert mux.get_output() == 1

    def test_MUX_16to1_100011111111111111101(self):
        mux = Multiplexer16To1(
            1, 0, 0, 0, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 0, 1)
        assert mux.get_output() == 0

    def test_MUX_16to1_100011111111111111111(self):
        mux = Multiplexer16To1(
            1, 0, 0, 0, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1)
        assert mux.get_output() == 1

    def test_MUX_16to1_100100000000000000000(self):
        mux = Multiplexer16To1(
            1, 0, 0, 1, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0)
        assert mux.get_output() == 0

    def test_MUX_16to1_100100000000000000100(self):
        mux = Multiplexer16To1(
            1, 0, 0, 1, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 1, 0, 0)
        assert mux.get_output() == 1

    def test_MUX_16to1_100101111111111111011(self):
        mux = Multiplexer16To1(
            1, 0, 0, 1, 0, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 0, 1, 1)
        assert mux.get_output() == 0

    def test_MUX_16to1_100101111111111111111(self):
        mux = Multiplexer16To1(
            1, 0, 0, 1, 0, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1)
        assert mux.get_output() == 1

    def test_MUX_16to1_100110000000000000000(self):
        mux = Multiplexer16To1(
            1, 0, 0, 1, 1, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0)
        assert mux.get_output() == 0

    def test_MUX_16to1_100110000000000001000(self):
        mux = Multiplexer16To1(
            1, 0, 0, 1, 1, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 1, 0, 0, 0)
        assert mux.get_output() == 1

    def test_MUX_16to1_100111111111111110111(self):
        mux = Multiplexer16To1(
            1, 0, 0, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 0, 1, 1, 1)
        assert mux.get_output() == 0

    def test_MUX_16to1_100111111111111111111(self):
        mux = Multiplexer16To1(
            1, 0, 0, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1)
        assert mux.get_output() == 1

    def test_MUX_16to1_101000000000000000000(self):
        mux = Multiplexer16To1(
            1, 0, 1, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0)
        assert mux.get_output() == 0

    def test_MUX_16to1_101001111111111110000(self):
        mux = Multiplexer16To1(
            1, 0, 1, 0, 0, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 0, 0, 0, 0)
        assert mux.get_output() == 1

    def test_MUX_16to1_101001111111111101111(self):
        mux = Multiplexer16To1(
            1, 0, 1, 0, 0, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 0, 1, 1, 1, 1)
        assert mux.get_output() == 0

    def test_MUX_16to1_101001111111111111111(self):
        mux = Multiplexer16To1(
            1, 0, 1, 0, 0, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1)
        assert mux.get_output() == 1

    def test_MUX_16to1_101010000000000000000(self):
        mux = Multiplexer16To1(
            1, 0, 1, 0, 1, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0)
        assert mux.get_output() == 0

    def test_MUX_16to1_101010000000000100000(self):
        mux = Multiplexer16To1(
            1, 0, 1, 0, 1, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 1, 0, 0, 0, 0, 0)
        assert mux.get_output() == 1

    def test_MUX_16to1_101011111111111011111(self):
        mux = Multiplexer16To1(
            1, 0, 1, 0, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 0, 1, 1, 1, 1, 1)
        assert mux.get_output() == 0

    def test_MUX_16to1_101011111111111111111(self):
        mux = Multiplexer16To1(
            1, 0, 1, 0, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1)
        assert mux.get_output() == 1

    def test_MUX_16to1_101100000000000000000(self):
        mux = Multiplexer16To1(
            1, 0, 1, 1, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0)
        assert mux.get_output() == 0

    def test_MUX_16to1_101100000000001000000(self):
        mux = Multiplexer16To1(
            1, 0, 1, 1, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 1, 0, 0, 0, 0, 0, 0)
        assert mux.get_output() == 1

    def test_MUX_16to1_101101111111110111111(self):
        mux = Multiplexer16To1(
            1, 0, 1, 1, 0, 1, 1, 1, 1, 1, 1, 1, 1, 1, 0, 1, 1, 1, 1, 1, 1)
        assert mux.get_output() == 0

    def test_MUX_16to1_101101111111111111111(self):
        mux = Multiplexer16To1(
            1, 0, 1, 1, 0, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1)
        assert mux.get_output() == 1

    def test_MUX_16to1_101110000000000000000(self):
        mux = Multiplexer16To1(
            1, 0, 1, 1, 1, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0)
        assert mux.get_output() == 0

    def test_MUX_16to1_101110000000010000000(self):
        mux = Multiplexer16To1(
            1, 0, 1, 1, 1, 0, 0, 0, 0, 0, 0, 0, 0, 1, 0, 0, 0, 0, 0, 0, 0)
        assert mux.get_output() == 1

    def test_MUX_16to1_101111111111101111111(self):
        mux = Multiplexer16To1(
            1, 0, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 0, 1, 1, 1, 1, 1, 1, 1)
        assert mux.get_output() == 0

    def test_MUX_16to1_101111111111111111111(self):
        mux = Multiplexer16To1(
            1, 0, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1)
        assert mux.get_output() == 1

    def test_MUX_16to1_110000000000000000000(self):
        mux = Multiplexer16To1(
            1, 1, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0)
        assert mux.get_output() == 0

    def test_MUX_16to1_110000000000100000000(self):
        mux = Multiplexer16To1(
            1, 1, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 1, 0, 0, 0, 0, 0, 0, 0, 0)
        assert mux.get_output() == 1

    def test_MUX_16to1_110001111111011111111(self):
        mux = Multiplexer16To1(
            1, 1, 0, 0, 0, 1, 1, 1, 1, 1, 1, 1, 0, 1, 1, 1, 1, 1, 1, 1, 1)
        assert mux.get_output() == 0

    def test_MUX_16to1_110001111111111111111(self):
        mux = Multiplexer16To1(
            1, 1, 0, 0, 0, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1)
        assert mux.get_output() == 1

    def test_MUX_16to1_110010000000000000000(self):
        mux = Multiplexer16To1(
            1, 1, 0, 0, 1, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0)
        assert mux.get_output() == 0

    def test_MUX_16to1_110010000001000000000(self):
        mux = Multiplexer16To1(
            1, 1, 0, 0, 1, 0, 0, 0, 0, 0, 0, 1, 0, 0, 0, 0, 0, 0, 0, 0, 0)
        assert mux.get_output() == 1

    def test_MUX_16to1_110011111110111111111(self):
        mux = Multiplexer16To1(
            1, 1, 0, 0, 1, 1, 1, 1, 1, 1, 1, 0, 1, 1, 1, 1, 1, 1, 1, 1, 1)
        assert mux.get_output() == 0

    def test_MUX_16to1_110011111111111111111(self):
        mux = Multiplexer16To1(
            1, 1, 0, 0, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1)
        assert mux.get_output() == 1

    def test_MUX_16to1_110100000000000000000(self):
        mux = Multiplexer16To1(
            1, 1, 0, 1, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0)
        assert mux.get_output() == 0

    def test_MUX_16to1_110100000010000000000(self):
        mux = Multiplexer16To1(
            1, 1, 0, 1, 0, 0, 0, 0, 0, 0, 1, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0)
        assert mux.get_output() == 1

    def test_MUX_16to1_110101111101111111111(self):
        mux = Multiplexer16To1(
            1, 1, 0, 1, 0, 1, 1, 1, 1, 1, 0, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1)
        assert mux.get_output() == 0

    def test_MUX_16to1_110101111111111111111(self):
        mux = Multiplexer16To1(
            1, 1, 0, 1, 0, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1)
        assert mux.get_output() == 1

    def test_MUX_16to1_110110000000000000000(self):
        mux = Multiplexer16To1(
            1, 1, 0, 1, 1, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0)
        assert mux.get_output() == 0

    def test_MUX_16to1_110110000100000000000(self):
        mux = Multiplexer16To1(
            1, 1, 0, 1, 1, 0, 0, 0, 0, 1, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0)
        assert mux.get_output() == 1

    def test_MUX_16to1_110111111011111111111(self):
        mux = Multiplexer16To1(
            1, 1, 0, 1, 1, 1, 1, 1, 1, 0, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1)
        assert mux.get_output() == 0

    def test_MUX_16to1_110111111111111111111(self):
        mux = Multiplexer16To1(
            1, 1, 0, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1)
        assert mux.get_output() == 1

    def test_MUX_16to1_111000000000000000000(self):
        mux = Multiplexer16To1(
            1, 1, 1, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0)
        assert mux.get_output() == 0

    def test_MUX_16to1_111000001000000000000(self):
        mux = Multiplexer16To1(
            1, 1, 1, 0, 0, 0, 0, 0, 1, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0)
        assert mux.get_output() == 1

    def test_MUX_16to1_111001110111111111111(self):
        mux = Multiplexer16To1(
            1, 1, 1, 0, 0, 1, 1, 1, 0, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1)
        assert mux.get_output() == 0

    def test_MUX_16to1_111001111111111111111(self):
        mux = Multiplexer16To1(
            1, 1, 1, 0, 0, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1)
        assert mux.get_output() == 1

    def test_MUX_16to1_111010000000000000000(self):
        mux = Multiplexer16To1(
            1, 1, 1, 0, 1, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0)
        assert mux.get_output() == 0

    def test_MUX_16to1_111010010000000000000(self):
        mux = Multiplexer16To1(
            1, 1, 1, 0, 1, 0, 0, 1, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0)
        assert mux.get_output() == 1

    def test_MUX_16to1_111011101111111111111(self):
        mux = Multiplexer16To1(
            1, 1, 1, 0, 1, 1, 1, 0, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1)
        assert mux.get_output() == 0

    def test_MUX_16to1_111011111111111111111(self):
        mux = Multiplexer16To1(
            1, 1, 1, 0, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1)
        assert mux.get_output() == 1

    def test_MUX_16to1_111100000000000000000(self):
        mux = Multiplexer16To1(
            1, 1, 1, 1, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0)
        assert mux.get_output() == 0

    def test_MUX_16to1_111100100000000000000(self):
        mux = Multiplexer16To1(
            1, 1, 1, 1, 0, 0, 1, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0)
        assert mux.get_output() == 1

    def test_MUX_16to1_111101011111111111111(self):
        mux = Multiplexer16To1(
            1, 1, 1, 1, 0, 1, 0, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1)
        assert mux.get_output() == 0

    def test_MUX_16to1_111101111111111111111(self):
        mux = Multiplexer16To1(
            1, 1, 1, 1, 0, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1)
        assert mux.get_output() == 1

    def test_MUX_16to1_111110000000000000000(self):
        mux = Multiplexer16To1(
            1, 1, 1, 1, 1, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0)
        assert mux.get_output() == 0

    def test_MUX_16to1_111111000000000000000(self):
        mux = Multiplexer16To1(
            1, 1, 1, 1, 1, 1, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0)
        assert mux.get_output() == 1

    def test_MUX_16to1_111110111111111111111(self):
        mux = Multiplexer16To1(
            1, 1, 1, 1, 1, 0, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1)
        assert mux.get_output() == 0

    def test_MUX_16to1_111111111111111111111(self):
        mux = Multiplexer16To1(
            1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1)
        assert mux.get_output() == 1
