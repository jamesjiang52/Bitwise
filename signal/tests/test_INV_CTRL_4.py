import sys
sys.path.insert(0, "../../")
import signal
from INV_CTRL import ControlledINV4


class TestControlledINV4:
    def test_INV_CTRL_4_00000(self):
        inv = ControlledINV4(0, 0, 0, 0, 0)
        assert inv.get_output() == (0, 0, 0, 0)

    def test_INV_CTRL_4_00001(self):
        inv = ControlledINV4(0, 0, 0, 0, 1)
        assert inv.get_output() == (0, 0, 0, 1)

    def test_INV_CTRL_4_00011(self):
        inv = ControlledINV4(0, 0, 0, 1, 1)
        assert inv.get_output() == (0, 0, 1, 1)

    def test_INV_CTRL_4_00111(self):
        inv = ControlledINV4(0, 0, 1, 1, 1)
        assert inv.get_output() == (0, 1, 1, 1)

    def test_INV_CTRL_4_01111(self):
        inv = ControlledINV4(0, 1, 1, 1, 1)
        assert inv.get_output() == (1, 1, 1, 1)

    def test_INV_CTRL_4_10000(self):
        inv = ControlledINV4(1, 0, 0, 0, 0)
        assert inv.get_output() == (1, 1, 1, 1)

    def test_INV_CTRL_4_10001(self):
        inv = ControlledINV4(1, 0, 0, 0, 1)
        assert inv.get_output() == (1, 1, 1, 0)

    def test_INV_CTRL_4_10011(self):
        inv = ControlledINV4(1, 0, 0, 1, 1)
        assert inv.get_output() == (1, 1, 0, 0)

    def test_INV_CTRL_4_10111(self):
        inv = ControlledINV4(1, 0, 1, 1, 1)
        assert inv.get_output() == (1, 0, 0, 0)

    def test_INV_CTRL_4_11111(self):
        inv = ControlledINV4(1, 1, 1, 1, 1)
        assert inv.get_output() == (0, 0, 0, 0)
