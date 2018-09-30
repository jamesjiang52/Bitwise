import sys
sys.path.insert(0, "../../")
import arithmetic
from ADD import HalfADD


class TestHalfADD:
    def test_ADD_half_00(self):
        ha = HalfADD(0, 0)
        assert ha.get_output() == (0, 0)

    def test_ADD_half_01(self):
        ha = HalfADD(0, 1)
        assert ha.get_output() == (0, 1)

    def test_ADD_half_10(self):
        ha = HalfADD(1, 0)
        assert ha.get_output() == (0, 1)

    def test_ADD_half_11(self):
        ha = HalfADD(1, 1)
        assert ha.get_output() == (1, 0)
