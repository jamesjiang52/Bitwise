import sys
sys.path.append("..")
from AND import AND


class TestAND:
    def test_AND_00(self):
        _and = AND(0, 0)
        assert _and.get_output() == 0

    def test_AND_01(self):
        _and = AND(0, 1)
        assert _and.get_output() == 0

    def test_AND_10(self):
        _and = AND(1, 0)
        assert _and.get_output() == 0

    def test_AND_11(self):
        _and = AND(1, 1)
        assert _and.get_output() == 1

    def test_AND_000(self):
        _and = AND(0, 0, 0)
        assert _and.get_output() == 0

    def test_AND_001(self):
        _and = AND(0, 0, 1)
        assert _and.get_output() == 0

    def test_AND_010(self):
        _and = AND(0, 1, 0)
        assert _and.get_output() == 0

    def test_AND_011(self):
        _and = AND(0, 1, 1)
        assert _and.get_output() == 0

    def test_AND_100(self):
        _and = AND(1, 0, 0)
        assert _and.get_output() == 0

    def test_AND_101(self):
        _and = AND(1, 0, 1)
        assert _and.get_output() == 0

    def test_AND_110(self):
        _and = AND(1, 1, 0)
        assert _and.get_output() == 0

    def test_AND_111(self):
        _and = AND(1, 1, 1)
        assert _and.get_output() == 1
