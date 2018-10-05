import sys
sys.path.insert(0, "../../")
import signal
from DEMUX import Demultiplexer1To4


class TestDemultiplexer1To4:
    def test_DEMUX_1to4_0000(self):
        demux = Demultiplexer1To4(0, 0, 0, 0)
        assert demux.get_output() == (0, 0, 0, 0)

    def test_DEMUX_1to4_0111(self):
        demux = Demultiplexer1To4(0, 1, 1, 1)
        assert demux.get_output() == (0, 0, 0, 0)

    def test_DEMUX_1to4_1000(self):
        demux = Demultiplexer1To4(1, 0, 0, 0)
        assert demux.get_output() == (0, 0, 0, 0)

    def test_DEMUX_1to4_1001(self):
        demux = Demultiplexer1To4(1, 0, 0, 1)
        assert demux.get_output() == (0, 0, 0, 1)

    def test_DEMUX_1to4_1010(self):
        demux = Demultiplexer1To4(1, 0, 1, 0)
        assert demux.get_output() == (0, 0, 0, 0)

    def test_DEMUX_1to4_1011(self):
        demux = Demultiplexer1To4(1, 0, 1, 1)
        assert demux.get_output() == (0, 0, 1, 0)

    def test_DEMUX_1to4_1100(self):
        demux = Demultiplexer1To4(1, 1, 0, 0)
        assert demux.get_output() == (0, 0, 0, 0)

    def test_DEMUX_1to4_1101(self):
        demux = Demultiplexer1To4(1, 1, 0, 1)
        assert demux.get_output() == (0, 1, 0, 0)

    def test_DEMUX_1to4_1110(self):
        demux = Demultiplexer1To4(1, 1, 1, 0)
        assert demux.get_output() == (0, 0, 0, 0)

    def test_DEMUX_1to4_1111(self):
        demux = Demultiplexer1To4(1, 1, 1, 1)
        assert demux.get_output() == (1, 0, 0, 0)
