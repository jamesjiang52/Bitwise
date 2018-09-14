import sys
sys.path.append("../")
import NAND


class TestNAND:
    # two inputs
    def test_NAND_00(self):
        _nand = NAND.NAND(0, 0)
        assert _nand.get_output() == 1

    def test_NAND_01(self):
        _nand = NAND.NAND(0, 1)
        assert _nand.get_output() == 1

    def test_NAND_10(self):
        _nand = NAND.NAND(1, 0)
        assert _nand.get_output() == 1

    def test_NAND_11(self):
        _nand = NAND.NAND(1, 1)
        assert _nand.get_output() == 0

    # three inputs
    def test_NAND_000(self):
        _nand = NAND.NAND(0, 0, 0)
        assert _nand.get_output() == 1

    def test_NAND_001(self):
        _nand = NAND.NAND(0, 0, 1)
        assert _nand.get_output() == 1

    def test_NAND_010(self):
        _nand = NAND.NAND(0, 1, 0)
        assert _nand.get_output() == 1

    def test_NAND_011(self):
        _nand = NAND.NAND(0, 1, 1)
        assert _nand.get_output() == 1

    def test_NAND_100(self):
        _nand = NAND.NAND(1, 0, 0)
        assert _nand.get_output() == 1

    def test_NAND_101(self):
        _nand = NAND.NAND(1, 0, 1)
        assert _nand.get_output() == 1

    def test_NAND_110(self):
        _nand = NAND.NAND(1, 1, 0)
        assert _nand.get_output() == 1

    def test_NAND_111(self):
        _nand = NAND.NAND(1, 1, 1)
        assert _nand.get_output() == 0
