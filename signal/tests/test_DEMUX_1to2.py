import sys
sys.path.insert(0, "../../")
import signal
from DEMUX import DEMUX1To2


class TestDEMUX1To2:
    def test_DEMUX_1to2_000(self):
        demux = DEMUX1To2(0, 0, 0)
        assert demux.get_output() == (0, 0)

    def test_DEMUX_1to2_011(self):
        demux = DEMUX1To2(0, 1, 1)
        assert demux.get_output() == (0, 0)

    def test_DEMUX_1to2_100(self):
        demux = DEMUX1To2(1, 0, 0)
        assert demux.get_output() == (0, 0)

    def test_DEMUX_1to2_101(self):
        demux = DEMUX1To2(1, 0, 1)
        assert demux.get_output() == (0, 1)

    def test_DEMUX_1to2_110(self):
        demux = DEMUX1To2(1, 1, 0)
        assert demux.get_output() == (0, 0)

    def test_DEMUX_1to2_111(self):
        demux = DEMUX1To2(1, 1, 1)
        assert demux.get_output() == (1, 0)
