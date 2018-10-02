import sys
sys.path.insert(0, "../../")
import signal
from INV_CTRL import ControlledINV16


class TestControlledINV16:
    def test_INV_CTRL_16_00000000000000000(self):
        inv = ControlledINV16(
            0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0)
        assert inv.get_output() == (
            0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0)

    def test_INV_CTRL_16_00000000000000001(self):
        inv = ControlledINV16(
            0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 1)
        assert inv.get_output() == (
            0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 1)

    def test_INV_CTRL_16_00000000000000011(self):
        inv = ControlledINV16(
            0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 1, 1)
        assert inv.get_output() == (
            0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 1, 1)

    def test_INV_CTRL_16_00000000000000111(self):
        inv = ControlledINV16(
            0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 1, 1, 1)
        assert inv.get_output() == (
            0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 1, 1, 1)

    def test_INV_CTRL_16_00000000000001111(self):
        inv = ControlledINV16(
            0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 1, 1, 1, 1)
        assert inv.get_output() == (
            0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 1, 1, 1, 1)

    def test_INV_CTRL_16_00000000000011111(self):
        inv = ControlledINV16(
            0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 1, 1, 1, 1, 1)
        assert inv.get_output() == (
            0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 1, 1, 1, 1, 1)

    def test_INV_CTRL_16_00000000000111111(self):
        inv = ControlledINV16(
            0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 1, 1, 1, 1, 1, 1)
        assert inv.get_output() == (
            0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 1, 1, 1, 1, 1, 1)

    def test_INV_CTRL_16_00000000001111111(self):
        inv = ControlledINV16(
            0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 1, 1, 1, 1, 1, 1, 1)
        assert inv.get_output() == (
            0, 0, 0, 0, 0, 0, 0, 0, 0, 1, 1, 1, 1, 1, 1, 1)

    def test_INV_CTRL_16_00000000011111111(self):
        inv = ControlledINV16(
            0, 0, 0, 0, 0, 0, 0, 0, 0, 1, 1, 1, 1, 1, 1, 1, 1)
        assert inv.get_output() == (
            0, 0, 0, 0, 0, 0, 0, 0, 1, 1, 1, 1, 1, 1, 1, 1)

    def test_INV_CTRL_16_00000000111111111(self):
        inv = ControlledINV16(
            0, 0, 0, 0, 0, 0, 0, 0, 1, 1, 1, 1, 1, 1, 1, 1, 1)
        assert inv.get_output() == (
            0, 0, 0, 0, 0, 0, 0, 1, 1, 1, 1, 1, 1, 1, 1, 1)

    def test_INV_CTRL_16_00000001111111111(self):
        inv = ControlledINV16(
            0, 0, 0, 0, 0, 0, 0, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1)
        assert inv.get_output() == (
            0, 0, 0, 0, 0, 0, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1)

    def test_INV_CTRL_16_00000011111111111(self):
        inv = ControlledINV16(
            0, 0, 0, 0, 0, 0, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1)
        assert inv.get_output() == (
            0, 0, 0, 0, 0, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1)

    def test_INV_CTRL_16_00000111111111111(self):
        inv = ControlledINV16(
            0, 0, 0, 0, 0, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1)
        assert inv.get_output() == (
            0, 0, 0, 0, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1)

    def test_INV_CTRL_16_00001111111111111(self):
        inv = ControlledINV16(
            0, 0, 0, 0, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1)
        assert inv.get_output() == (
            0, 0, 0, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1)

    def test_INV_CTRL_16_00011111111111111(self):
        inv = ControlledINV16(
            0, 0, 0, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1)
        assert inv.get_output() == (
            0, 0, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1)

    def test_INV_CTRL_16_00111111111111111(self):
        inv = ControlledINV16(
            0, 0, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1)
        assert inv.get_output() == (
            0, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1)

    def test_INV_CTRL_16_01111111111111111(self):
        inv = ControlledINV16(
            0, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1)
        assert inv.get_output() == (
            1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1)

    def test_INV_CTRL_16_10000000000000000(self):
        inv = ControlledINV16(
            1, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0)
        assert inv.get_output() == (
            1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1)

    def test_INV_CTRL_16_10000000000000001(self):
        inv = ControlledINV16(
            1, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 1)
        assert inv.get_output() == (
            1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 0)

    def test_INV_CTRL_16_10000000000000011(self):
        inv = ControlledINV16(
            1, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 1, 1)
        assert inv.get_output() == (
            1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 0, 0)

    def test_INV_CTRL_16_10000000000000111(self):
        inv = ControlledINV16(
            1, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 1, 1, 1)
        assert inv.get_output() == (
            1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 0, 0, 0)

    def test_INV_CTRL_16_10000000000001111(self):
        inv = ControlledINV16(
            1, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 1, 1, 1, 1)
        assert inv.get_output() == (
            1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 0, 0, 0, 0)

    def test_INV_CTRL_16_10000000000011111(self):
        inv = ControlledINV16(
            1, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 1, 1, 1, 1, 1)
        assert inv.get_output() == (
            1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 0, 0, 0, 0, 0)

    def test_INV_CTRL_16_10000000000111111(self):
        inv = ControlledINV16(
            1, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 1, 1, 1, 1, 1, 1)
        assert inv.get_output() == (
            1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 0, 0, 0, 0, 0, 0)

    def test_INV_CTRL_16_10000000001111111(self):
        inv = ControlledINV16(
            1, 0, 0, 0, 0, 0, 0, 0, 0, 0, 1, 1, 1, 1, 1, 1, 1)
        assert inv.get_output() == (
            1, 1, 1, 1, 1, 1, 1, 1, 1, 0, 0, 0, 0, 0, 0, 0)

    def test_INV_CTRL_16_10000000011111111(self):
        inv = ControlledINV16(
            1, 0, 0, 0, 0, 0, 0, 0, 0, 1, 1, 1, 1, 1, 1, 1, 1)
        assert inv.get_output() == (
            1, 1, 1, 1, 1, 1, 1, 1, 0, 0, 0, 0, 0, 0, 0, 0)

    def test_INV_CTRL_16_10000000111111111(self):
        inv = ControlledINV16(
            1, 0, 0, 0, 0, 0, 0, 0, 1, 1, 1, 1, 1, 1, 1, 1, 1)
        assert inv.get_output() == (
            1, 1, 1, 1, 1, 1, 1, 0, 0, 0, 0, 0, 0, 0, 0, 0)

    def test_INV_CTRL_16_10000001111111111(self):
        inv = ControlledINV16(
            1, 0, 0, 0, 0, 0, 0, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1)
        assert inv.get_output() == (
            1, 1, 1, 1, 1, 1, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0)

    def test_INV_CTRL_16_10000011111111111(self):
        inv = ControlledINV16(
            1, 0, 0, 0, 0, 0, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1)
        assert inv.get_output() == (
            1, 1, 1, 1, 1, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0)

    def test_INV_CTRL_16_10000111111111111(self):
        inv = ControlledINV16(
            1, 0, 0, 0, 0, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1)
        assert inv.get_output() == (
            1, 1, 1, 1, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0)

    def test_INV_CTRL_16_10001111111111111(self):
        inv = ControlledINV16(
            1, 0, 0, 0, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1)
        assert inv.get_output() == (
            1, 1, 1, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0)

    def test_INV_CTRL_16_10011111111111111(self):
        inv = ControlledINV16(
            1, 0, 0, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1)
        assert inv.get_output() == (
            1, 1, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0)

    def test_INV_CTRL_16_10111111111111111(self):
        inv = ControlledINV16(
            1, 0, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1)
        assert inv.get_output() == (
            1, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0)

    def test_INV_CTRL_16_11111111111111111(self):
        inv = ControlledINV16(
            1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1)
        assert inv.get_output() == (
            0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0)
