import sys
sys.path.append("../")
import NOR


class TestNOR:
    # two inputs
    def test_NOR_00(self):
        _nor = NOR.NOR(0, 0)
        assert _nor.get_output() == 1

    def test_NOR_01(self):
        _nor = NOR.NOR(0, 1)
        assert _nor.get_output() == 0

    def test_NOR_10(self):
        _nor = NOR.NOR(1, 0)
        assert _nor.get_output() == 0

    def test_NOR_11(self):
        _nor = NOR.NOR(1, 1)
        assert _nor.get_output() == 0

    # three inputs
    def test_NOR_000(self):
        _nor = NOR.NOR(0, 0, 0)
        assert _nor.get_output() == 1

    def test_NOR_001(self):
        _nor = NOR.NOR(0, 0, 1)
        assert _nor.get_output() == 0

    def test_NOR_010(self):
        _nor = NOR.NOR(0, 1, 0)
        assert _nor.get_output() == 0

    def test_NOR_011(self):
        _nor = NOR.NOR(0, 1, 1)
        assert _nor.get_output() == 0

    def test_NOR_100(self):
        _nor = NOR.NOR(1, 0, 0)
        assert _nor.get_output() == 0

    def test_NOR_101(self):
        _nor = NOR.NOR(1, 0, 1)
        assert _nor.get_output() == 0

    def test_NOR_110(self):
        _nor = NOR.NOR(1, 1, 0)
        assert _nor.get_output() == 0

    def test_NOR_111(self):
        _nor = NOR.NOR(1, 1, 1)
        assert _nor.get_output() == 0
