import sys
sys.path.append("..")
from NOT import NOT


class TestNOT:
    def test_NOT_0(self):
        _not = NOT(0)
        assert _not.get_output() == 1

    def test_NOT_1(self):
        _not = NOT(1)
        assert _not.get_output() == 0
