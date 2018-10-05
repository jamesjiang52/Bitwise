import sys
sys.path.insert(0, "../../")
import signal
from DEMUX import Demultiplexer1To16


class TestDemultiplexer1To16:
    def test_DEMUX_1to16_000000(self):
        demux = Demultiplexer1To16(0, 0, 0, 0, 0, 0)
        assert demux.get_output() == (
            0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0)

    def test_DEMUX_1to16_011111(self):
        demux = Demultiplexer1To16(0, 1, 1, 1, 1, 1)
        assert demux.get_output() == (
            0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0)

    def test_DEMUX_1to16_100000(self):
        demux = Demultiplexer1To16(1, 0, 0, 0, 0, 0)
        assert demux.get_output() == (
            0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0)

    def test_DEMUX_1to16_100001(self):
        demux = Demultiplexer1To16(1, 0, 0, 0, 0, 1)
        assert demux.get_output() == (
            0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 1)

    def test_DEMUX_1to16_100010(self):
        demux = Demultiplexer1To16(1, 0, 0, 0, 1, 0)
        assert demux.get_output() == (
            0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0)

    def test_DEMUX_1to16_100011(self):
        demux = Demultiplexer1To16(1, 0, 0, 0, 1, 1)
        assert demux.get_output() == (
            0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 1, 0)

    def test_DEMUX_1to16_100100(self):
        demux = Demultiplexer1To16(1, 0, 0, 1, 0, 0)
        assert demux.get_output() == (
            0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0)

    def test_DEMUX_1to16_100101(self):
        demux = Demultiplexer1To16(1, 0, 0, 1, 0, 1)
        assert demux.get_output() == (
            0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 1, 0, 0)

    def test_DEMUX_1to16_100110(self):
        demux = Demultiplexer1To16(1, 0, 0, 1, 1, 0)
        assert demux.get_output() == (
            0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0)

    def test_DEMUX_1to16_100111(self):
        demux = Demultiplexer1To16(1, 0, 0, 1, 1, 1)
        assert demux.get_output() == (
            0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 1, 0, 0, 0)

    def test_DEMUX_1to16_101000(self):
        demux = Demultiplexer1To16(1, 0, 1, 0, 0, 0)
        assert demux.get_output() == (
            0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0)

    def test_DEMUX_1to16_101001(self):
        demux = Demultiplexer1To16(1, 0, 1, 0, 0, 1)
        assert demux.get_output() == (
            0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 1, 0, 0, 0, 0)

    def test_DEMUX_1to16_101010(self):
        demux = Demultiplexer1To16(1, 0, 1, 0, 1, 0)
        assert demux.get_output() == (
            0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0)

    def test_DEMUX_1to16_101011(self):
        demux = Demultiplexer1To16(1, 0, 1, 0, 1, 1)
        assert demux.get_output() == (
            0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 1, 0, 0, 0, 0, 0)

    def test_DEMUX_1to16_101100(self):
        demux = Demultiplexer1To16(1, 0, 1, 1, 0, 0)
        assert demux.get_output() == (
            0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0)

    def test_DEMUX_1to16_101101(self):
        demux = Demultiplexer1To16(1, 0, 1, 1, 0, 1)
        assert demux.get_output() == (
            0, 0, 0, 0, 0, 0, 0, 0, 0, 1, 0, 0, 0, 0, 0, 0)

    def test_DEMUX_1to16_101110(self):
        demux = Demultiplexer1To16(1, 0, 1, 1, 1, 0)
        assert demux.get_output() == (
            0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0)

    def test_DEMUX_1to16_101111(self):
        demux = Demultiplexer1To16(1, 0, 1, 1, 1, 1)
        assert demux.get_output() == (
            0, 0, 0, 0, 0, 0, 0, 0, 1, 0, 0, 0, 0, 0, 0, 0)

    def test_DEMUX_1to16_110000(self):
        demux = Demultiplexer1To16(1, 1, 0, 0, 0, 0)
        assert demux.get_output() == (
            0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0)

    def test_DEMUX_1to16_110001(self):
        demux = Demultiplexer1To16(1, 1, 0, 0, 0, 1)
        assert demux.get_output() == (
            0, 0, 0, 0, 0, 0, 0, 1, 0, 0, 0, 0, 0, 0, 0, 0)

    def test_DEMUX_1to16_110010(self):
        demux = Demultiplexer1To16(1, 1, 0, 0, 1, 0)
        assert demux.get_output() == (
            0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0)

    def test_DEMUX_1to16_110011(self):
        demux = Demultiplexer1To16(1, 1, 0, 0, 1, 1)
        assert demux.get_output() == (
            0, 0, 0, 0, 0, 0, 1, 0, 0, 0, 0, 0, 0, 0, 0, 0)

    def test_DEMUX_1to16_110100(self):
        demux = Demultiplexer1To16(1, 1, 0, 1, 0, 0)
        assert demux.get_output() == (
            0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0)

    def test_DEMUX_1to16_110101(self):
        demux = Demultiplexer1To16(1, 1, 0, 1, 0, 1)
        assert demux.get_output() == (
            0, 0, 0, 0, 0, 1, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0)

    def test_DEMUX_1to16_110110(self):
        demux = Demultiplexer1To16(1, 1, 0, 1, 1, 0)
        assert demux.get_output() == (
            0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0)

    def test_DEMUX_1to16_110111(self):
        demux = Demultiplexer1To16(1, 1, 0, 1, 1, 1)
        assert demux.get_output() == (
            0, 0, 0, 0, 1, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0)

    def test_DEMUX_1to16_111000(self):
        demux = Demultiplexer1To16(1, 1, 1, 0, 0, 0)
        assert demux.get_output() == (
            0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0)

    def test_DEMUX_1to16_111001(self):
        demux = Demultiplexer1To16(1, 1, 1, 0, 0, 1)
        assert demux.get_output() == (
            0, 0, 0, 1, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0)

    def test_DEMUX_1to16_111010(self):
        demux = Demultiplexer1To16(1, 1, 1, 0, 1, 0)
        assert demux.get_output() == (
            0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0)

    def test_DEMUX_1to16_111011(self):
        demux = Demultiplexer1To16(1, 1, 1, 0, 1, 1)
        assert demux.get_output() == (
            0, 0, 1, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0)

    def test_DEMUX_1to16_111100(self):
        demux = Demultiplexer1To16(1, 1, 1, 1, 0, 0)
        assert demux.get_output() == (
            0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0)

    def test_DEMUX_1to16_111101(self):
        demux = Demultiplexer1To16(1, 1, 1, 1, 0, 1)
        assert demux.get_output() == (
            0, 1, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0)

    def test_DEMUX_1to16_111110(self):
        demux = Demultiplexer1To16(1, 1, 1, 1, 1, 0)
        assert demux.get_output() == (
            0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0)

    def test_DEMUX_1to16_111111(self):
        demux = Demultiplexer1To16(1, 1, 1, 1, 1, 1)
        assert demux.get_output() == (
            1, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0)

