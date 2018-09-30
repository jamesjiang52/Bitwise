import sys
sys.path.insert(0, "../../")
import gate
from NOR import NOR


class TestNOR:
    def test_NOR_00(self):
        nor = NOR(0, 0)
        assert nor.get_output() == 1

    def test_NOR_01(self):
        nor = NOR(0, 1)
        assert nor.get_output() == 0

    def test_NOR_10(self):
        nor = NOR(1, 0)
        assert nor.get_output() == 0

    def test_NOR_11(self):
        nor = NOR(1, 1)
        assert nor.get_output() == 0

    def test_NOR_000(self):
        nor = NOR(0, 0, 0)
        assert nor.get_output() == 1

    def test_NOR_001(self):
        nor = NOR(0, 0, 1)
        assert nor.get_output() == 0

    def test_NOR_010(self):
        nor = NOR(0, 1, 0)
        assert nor.get_output() == 0

    def test_NOR_011(self):
        nor = NOR(0, 1, 1)
        assert nor.get_output() == 0

    def test_NOR_100(self):
        nor = NOR(1, 0, 0)
        assert nor.get_output() == 0

    def test_NOR_101(self):
        nor = NOR(1, 0, 1)
        assert nor.get_output() == 0

    def test_NOR_110(self):
        nor = NOR(1, 1, 0)
        assert nor.get_output() == 0

    def test_NOR_111(self):
        nor = NOR(1, 1, 1)
        assert nor.get_output() == 0
