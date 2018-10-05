import sys
sys.path.insert(0, "../../")
import arithmetic
from ADD import FullAdder


class TestFullADD:
    def test_ADD_full_000(self):
        fa = FullAdder(0, 0, 0)
        assert fa.get_output() == (0, 0)

    def test_ADD_full_001(self):
        fa = FullAdder(0, 0, 1)
        assert fa.get_output() == (0, 1)

    def test_ADD_full_010(self):
        fa = FullAdder(0, 1, 0)
        assert fa.get_output() == (0, 1)

    def test_ADD_full_011(self):
        fa = FullAdder(0, 1, 1)
        assert fa.get_output() == (1, 0)

    def test_ADD_full_100(self):
        fa = FullAdder(1, 0, 0)
        assert fa.get_output() == (0, 1)

    def test_ADD_full_101(self):
        fa = FullAdder(1, 0, 1)
        assert fa.get_output() == (1, 0)

    def test_ADD_full_110(self):
        fa = FullAdder(1, 1, 0)
        assert fa.get_output() == (1, 0)

    def test_ADD_full_111(self):
        fa = FullAdder(1, 1, 1)
        assert fa.get_output() == (1, 1)
