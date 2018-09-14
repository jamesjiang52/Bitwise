import sys
sys.path.append("../")
import OR


class TestOR:
    # two inputs
    def test_OR_00(self):
        _or = OR.OR(0, 0)
        assert _or.get_output() == 0

    def test_OR_01(self):
        _or = OR.OR(0, 1)
        assert _or.get_output() == 1

    def test_OR_10(self):
        _or = OR.OR(1, 0)
        assert _or.get_output() == 1

    def test_OR_11(self):
        _or = OR.OR(1, 1)
        assert _or.get_output() == 1

    # three inputs
    def test_OR_000(self):
        _or = OR.OR(0, 0, 0)
        assert _or.get_output() == 0

    def test_OR_001(self):
        _or = OR.OR(0, 0, 1)
        assert _or.get_output() == 1

    def test_NR_010(self):
        _or = OR.OR(0, 1, 0)
        assert _or.get_output() == 1

    def test_OR_011(self):
        _or = OR.OR(0, 1, 1)
        assert _or.get_output() == 1

    def test_OR_100(self):
        _or = OR.OR(1, 0, 0)
        assert _or.get_output() == 1

    def test_OR_101(self):
        _or = OR.OR(1, 0, 1)
        assert _or.get_output() == 1

    def test_OR_110(self):
        _or = OR.OR(1, 1, 0)
        assert _or.get_output() == 1

    def test_OR_111(self):
        _or = OR.OR(1, 1, 1)
        assert _or.get_output() == 1
