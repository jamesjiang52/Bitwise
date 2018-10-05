import sys
sys.path.insert(0, "../../")
import signal
from DEMUX import Demultiplexer1To8


class TestDemultiplexer1To8:
    def test_DEMUX_1to8_00000(self):
        demux = Demultiplexer1To8(0, 0, 0, 0, 0)
        assert demux.get_output() == (0, 0, 0, 0, 0, 0, 0, 0)

    def test_DEMUX_1to8_01111(self):
        demux = Demultiplexer1To8(0, 1, 1, 1, 1)
        assert demux.get_output() == (0, 0, 0, 0, 0, 0, 0, 0)

    def test_DEMUX_1to8_10000(self):
        demux = Demultiplexer1To8(1, 0, 0, 0, 0)
        assert demux.get_output() == (0, 0, 0, 0, 0, 0, 0, 0)

    def test_DEMUX_1to8_10001(self):
        demux = Demultiplexer1To8(1, 0, 0, 0, 1)
        assert demux.get_output() == (0, 0, 0, 0, 0, 0, 0, 1)

    def test_DEMUX_1to8_10010(self):
        demux = Demultiplexer1To8(1, 0, 0, 1, 0)
        assert demux.get_output() == (0, 0, 0, 0, 0, 0, 0, 0)

    def test_DEMUX_1to8_10011(self):
        demux = Demultiplexer1To8(1, 0, 0, 1, 1)
        assert demux.get_output() == (0, 0, 0, 0, 0, 0, 1, 0)

    def test_DEMUX_1to8_10100(self):
        demux = Demultiplexer1To8(1, 0, 1, 0, 0)
        assert demux.get_output() == (0, 0, 0, 0, 0, 0, 0, 0)

    def test_DEMUX_1to8_10101(self):
        demux = Demultiplexer1To8(1, 0, 1, 0, 1)
        assert demux.get_output() == (0, 0, 0, 0, 0, 1, 0, 0)

    def test_DEMUX_1to8_10110(self):
        demux = Demultiplexer1To8(1, 0, 1, 1, 0)
        assert demux.get_output() == (0, 0, 0, 0, 0, 0, 0, 0)

    def test_DEMUX_1to8_10111(self):
        demux = Demultiplexer1To8(1, 0, 1, 1, 1)
        assert demux.get_output() == (0, 0, 0, 0, 1, 0, 0, 0)

    def test_DEMUX_1to8_11000(self):
        demux = Demultiplexer1To8(1, 1, 0, 0, 0)
        assert demux.get_output() == (0, 0, 0, 0, 0, 0, 0, 0)

    def test_DEMUX_1to8_11001(self):
        demux = Demultiplexer1To8(1, 1, 0, 0, 1)
        assert demux.get_output() == (0, 0, 0, 1, 0, 0, 0, 0)

    def test_DEMUX_1to8_11010(self):
        demux = Demultiplexer1To8(1, 1, 0, 1, 0)
        assert demux.get_output() == (0, 0, 0, 0, 0, 0, 0, 0)

    def test_DEMUX_1to8_11011(self):
        demux = Demultiplexer1To8(1, 1, 0, 1, 1)
        assert demux.get_output() == (0, 0, 1, 0, 0, 0, 0, 0)

    def test_DEMUX_1to8_11100(self):
        demux = Demultiplexer1To8(1, 1, 1, 0, 0)
        assert demux.get_output() == (0, 0, 0, 0, 0, 0, 0, 0)

    def test_DEMUX_1to8_11101(self):
        demux = Demultiplexer1To8(1, 1, 1, 0, 1)
        assert demux.get_output() == (0, 1, 0, 0, 0, 0, 0, 0)

    def test_DEMUX_1to8_11110(self):
        demux = Demultiplexer1To8(1, 1, 1, 1, 0)
        assert demux.get_output() == (0, 0, 0, 0, 0, 0, 0, 0)

    def test_DEMUX_1to8_11111(self):
        demux = Demultiplexer1To8(1, 1, 1, 1, 1)
        assert demux.get_output() == (1, 0, 0, 0, 0, 0, 0, 0)
