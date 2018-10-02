import sys
sys.path.insert(0, "../../")
import signal
from INV_CTRL import ControlledINV8


class TestControlledINV8:
    def test_INV_CTRL_8_000000000(self):
        inv = ControlledINV8(0, 0, 0, 0, 0, 0, 0, 0, 0)
        assert inv.get_output() == (0, 0, 0, 0, 0, 0, 0, 0)

    def test_INV_CTRL_8_000000001(self):
        inv = ControlledINV8(0, 0, 0, 0, 0, 0, 0, 0, 1)
        assert inv.get_output() == (0, 0, 0, 0, 0, 0, 0, 1)

    def test_INV_CTRL_8_000000011(self):
        inv = ControlledINV8(0, 0, 0, 0, 0, 0, 0, 1, 1)
        assert inv.get_output() == (0, 0, 0, 0, 0, 0, 1, 1)

    def test_INV_CTRL_8_000000111(self):
        inv = ControlledINV8(0, 0, 0, 0, 0, 0, 1, 1, 1)
        assert inv.get_output() == (0, 0, 0, 0, 0, 1, 1, 1)

    def test_INV_CTRL_8_000001111(self):
        inv = ControlledINV8(0, 0, 0, 0, 0, 1, 1, 1, 1)
        assert inv.get_output() == (0, 0, 0, 0, 1, 1, 1, 1)

    def test_INV_CTRL_8_000011111(self):
        inv = ControlledINV8(0, 0, 0, 0, 1, 1, 1, 1, 1)
        assert inv.get_output() == (0, 0, 0, 1, 1, 1, 1, 1)

    def test_INV_CTRL_8_000111111(self):
        inv = ControlledINV8(0, 0, 0, 1, 1, 1, 1, 1, 1)
        assert inv.get_output() == (0, 0, 1, 1, 1, 1, 1, 1)

    def test_INV_CTRL_8_001111111(self):
        inv = ControlledINV8(0, 0, 1, 1, 1, 1, 1, 1, 1)
        assert inv.get_output() == (0, 1, 1, 1, 1, 1, 1, 1)

    def test_INV_CTRL_8_011111111(self):
        inv = ControlledINV8(0, 1, 1, 1, 1, 1, 1, 1, 1)
        assert inv.get_output() == (1, 1, 1, 1, 1, 1, 1, 1)

    def test_INV_CTRL_8_100000000(self):
        inv = ControlledINV8(1, 0, 0, 0, 0, 0, 0, 0, 0)
        assert inv.get_output() == (1, 1, 1, 1, 1, 1, 1, 1)

    def test_INV_CTRL_8_100000001(self):
        inv = ControlledINV8(1, 0, 0, 0, 0, 0, 0, 0, 1)
        assert inv.get_output() == (1, 1, 1, 1, 1, 1, 1, 0)

    def test_INV_CTRL_8_100000011(self):
        inv = ControlledINV8(1, 0, 0, 0, 0, 0, 0, 1, 1)
        assert inv.get_output() == (1, 1, 1, 1, 1, 1, 0, 0)

    def test_INV_CTRL_8_100000111(self):
        inv = ControlledINV8(1, 0, 0, 0, 0, 0, 1, 1, 1)
        assert inv.get_output() == (1, 1, 1, 1, 1, 0, 0, 0)

    def test_INV_CTRL_8_100001111(self):
        inv = ControlledINV8(1, 0, 0, 0, 0, 1, 1, 1, 1)
        assert inv.get_output() == (1, 1, 1, 1, 0, 0, 0, 0)

    def test_INV_CTRL_8_100011111(self):
        inv = ControlledINV8(1, 0, 0, 0, 1, 1, 1, 1, 1)
        assert inv.get_output() == (1, 1, 1, 0, 0, 0, 0, 0)

    def test_INV_CTRL_8_100111111(self):
        inv = ControlledINV8(1, 0, 0, 1, 1, 1, 1, 1, 1)
        assert inv.get_output() == (1, 1, 0, 0, 0, 0, 0, 0)

    def test_INV_CTRL_8_101111111(self):
        inv = ControlledINV8(1, 0, 1, 1, 1, 1, 1, 1, 1)
        assert inv.get_output() == (1, 0, 0, 0, 0, 0, 0, 0)

    def test_INV_CTRL_8_111111111(self):
        inv = ControlledINV8(1, 1, 1, 1, 1, 1, 1, 1, 1)
        assert inv.get_output() == (0, 0, 0, 0, 0, 0, 0, 0)
