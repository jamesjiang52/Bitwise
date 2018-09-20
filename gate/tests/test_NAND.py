import sys
sys.path.append("..")
from NAND import NAND


class TestNAND:
    def test_NAND_00(self):
        nand = NAND(0, 0)
        assert nand.get_output() == 1

    def test_NAND_01(self):
        nand = NAND(0, 1)
        assert nand.get_output() == 1

    def test_NAND_10(self):
        nand = NAND(1, 0)
        assert nand.get_output() == 1

    def test_NAND_11(self):
        nand = NAND(1, 1)
        assert nand.get_output() == 0

    def test_NAND_000(self):
        nand = NAND(0, 0, 0)
        assert nand.get_output() == 1

    def test_NAND_001(self):
        nand = NAND(0, 0, 1)
        assert nand.get_output() == 1

    def test_NAND_010(self):
        nand = NAND(0, 1, 0)
        assert nand.get_output() == 1

    def test_nandNAND_011(self):
        nand = NAND(0, 1, 1)
        assert nand.get_output() == 1

    def test_NAND_100(self):
        nand = NAND(1, 0, 0)
        assert nand.get_output() == 1

    def test_NAND_101(self):
        nand = NAND(1, 0, 1)
        assert nand.get_output() == 1

    def test_NAND_110(self):
        nand = NAND(1, 1, 0)
        assert nand.get_output() == 1

    def test_NAND_111(self):
        nand = NAND(1, 1, 1)
        assert nand.get_output() == 0
