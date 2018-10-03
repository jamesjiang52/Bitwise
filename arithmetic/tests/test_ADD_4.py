import sys
sys.path.insert(0, "../../")
import arithmetic
from ADD import ADD4


class TestADD4:
    def test_ADD_4_000000000(self):
        add = ADD4(0, 0, 0, 0, 0, 0, 0, 0, 0)
        assert add.get_output() == (0, 0, 0, 0, 0)

    def test_ADD_4_000010001(self):
        add = ADD4(0, 0, 0, 0, 1, 0, 0, 0, 1)
        assert add.get_output() == (0, 0, 0, 1, 0)

    def test_ADD_4_000100010(self):
        add = ADD4(0, 0, 0, 1, 0, 0, 0, 1, 0)
        assert add.get_output() == (0, 0, 1, 0, 0)

    def test_ADD_4_001000100(self):
        add = ADD4(0, 0, 1, 0, 0, 0, 1, 0, 0)
        assert add.get_output() == (0, 1, 0, 0, 0)

    def test_ADD_4_010001000(self):
        add = ADD4(0, 1, 0, 0, 0, 1, 0, 0, 0)
        assert add.get_output() == (1, 0, 0, 0, 0)

    def test_ADD_4_000000001(self):
        add = ADD4(0, 0, 0, 0, 0, 0, 0, 0, 1)
        assert add.get_output() == (0, 0, 0, 0, 1)

    def test_ADD_4_000000010(self):
        add = ADD4(0, 0, 0, 0, 0, 0, 0, 1, 0)
        assert add.get_output() == (0, 0, 0, 1, 0)

    def test_ADD_4_000000100(self):
        add = ADD4(0, 0, 0, 0, 0, 0, 1, 0, 0)
        assert add.get_output() == (0, 0, 1, 0, 0)

    def test_ADD_4_000001000(self):
        add = ADD4(0, 0, 0, 0, 0, 1, 0, 0, 0)
        assert add.get_output() == (0, 1, 0, 0, 0)

    def test_ADD_4_000010000(self):
        add = ADD4(0, 0, 0, 0, 1, 0, 0, 0, 0)
        assert add.get_output() == (0, 0, 0, 0, 1)

    def test_ADD_4_000100000(self):
        add = ADD4(0, 0, 0, 1, 0, 0, 0, 0, 0)
        assert add.get_output() == (0, 0, 0, 1, 0)

    def test_ADD_4_001000000(self):
        add = ADD4(0, 0, 1, 0, 0, 0, 0, 0, 0)
        assert add.get_output() == (0, 0, 1, 0, 0)

    def test_ADD_4_010000000(self):
        add = ADD4(0, 1, 0, 0, 0, 0, 0, 0, 0)
        assert add.get_output() == (0, 1, 0, 0, 0)

    def test_ADD_4_011110000(self):
        add = ADD4(0, 1, 1, 1, 1, 0, 0, 0, 0)
        assert add.get_output() == (0, 1, 1, 1, 1)

    def test_ADD_4_011110001(self):
        add = ADD4(0, 1, 1, 1, 1, 0, 0, 0, 1)
        assert add.get_output() == (1, 0, 0, 0, 0)

    def test_ADD_4_011110010(self):
        add = ADD4(0, 1, 1, 1, 1, 0, 0, 1, 0)
        assert add.get_output() == (1, 0, 0, 0, 1)

    def test_ADD_4_011110100(self):
        add = ADD4(0, 1, 1, 1, 1, 0, 1, 0, 0)
        assert add.get_output() == (1, 0, 0, 1, 1)

    def test_ADD_4_011111000(self):
        add = ADD4(0, 1, 1, 1, 1, 1, 0, 0, 0)
        assert add.get_output() == (1, 0, 1, 1, 1)

    def test_ADD_4_000001111(self):
        add = ADD4(0, 0, 0, 0, 0, 1, 1, 1, 1)
        assert add.get_output() == (0, 1, 1, 1, 1)

    def test_ADD_4_000011111(self):
        add = ADD4(0, 0, 0, 0, 1, 1, 1, 1, 1)
        assert add.get_output() == (1, 0, 0, 0, 0)

    def test_ADD_4_000101111(self):
        add = ADD4(0, 0, 0, 1, 0, 1, 1, 1, 1)
        assert add.get_output() == (1, 0, 0, 0, 1)

    def test_ADD_4_001001111(self):
        add = ADD4(0, 0, 1, 0, 0, 1, 1, 1, 1)
        assert add.get_output() == (1, 0, 0, 1, 1)

    def test_ADD_4_010001111(self):
        add = ADD4(0, 1, 0, 0, 0, 1, 1, 1, 1)
        assert add.get_output() == (1, 0, 1, 1, 1)

    def test_ADD_4_011111111(self):
        add = ADD4(0, 1, 1, 1, 1, 1, 1, 1, 1)
        assert add.get_output() == (1, 1, 1, 1, 0)

    def test_ADD_4_100000000(self):
        add = ADD4(1, 0, 0, 0, 0, 0, 0, 0, 0)
        assert add.get_output() == (0, 0, 0, 0, 1)

    def test_ADD_4_100010001(self):
        add = ADD4(1, 0, 0, 0, 1, 0, 0, 0, 1)
        assert add.get_output() == (0, 0, 0, 1, 1)

    def test_ADD_4_100100010(self):
        add = ADD4(1, 0, 0, 1, 0, 0, 0, 1, 0)
        assert add.get_output() == (0, 0, 1, 0, 1)

    def test_ADD_4_101000100(self):
        add = ADD4(1, 0, 1, 0, 0, 0, 1, 0, 0)
        assert add.get_output() == (0, 1, 0, 0, 1)

    def test_ADD_4_110001000(self):
        add = ADD4(1, 1, 0, 0, 0, 1, 0, 0, 0)
        assert add.get_output() == (1, 0, 0, 0, 1)

    def test_ADD_4_100000001(self):
        add = ADD4(1, 0, 0, 0, 0, 0, 0, 0, 1)
        assert add.get_output() == (0, 0, 0, 1, 0)

    def test_ADD_4_100000010(self):
        add = ADD4(1, 0, 0, 0, 0, 0, 0, 1, 0)
        assert add.get_output() == (0, 0, 0, 1, 1)

    def test_ADD_4_100000100(self):
        add = ADD4(1, 0, 0, 0, 0, 0, 1, 0, 0)
        assert add.get_output() == (0, 0, 1, 0, 1)

    def test_ADD_4_100001000(self):
        add = ADD4(1, 0, 0, 0, 0, 1, 0, 0, 0)
        assert add.get_output() == (0, 1, 0, 0, 1)

    def test_ADD_4_100010000(self):
        add = ADD4(1, 0, 0, 0, 1, 0, 0, 0, 0)
        assert add.get_output() == (0, 0, 0, 1, 0)

    def test_ADD_4_100100000(self):
        add = ADD4(1, 0, 0, 1, 0, 0, 0, 0, 0)
        assert add.get_output() == (0, 0, 0, 1, 1)

    def test_ADD_4_101000000(self):
        add = ADD4(1, 0, 1, 0, 0, 0, 0, 0, 0)
        assert add.get_output() == (0, 0, 1, 0, 1)

    def test_ADD_4_110000000(self):
        add = ADD4(1, 1, 0, 0, 0, 0, 0, 0, 0)
        assert add.get_output() == (0, 1, 0, 0, 1)

    def test_ADD_4_111110000(self):
        add = ADD4(1, 1, 1, 1, 1, 0, 0, 0, 0)
        assert add.get_output() == (1, 0, 0, 0, 0)

    def test_ADD_4_111110001(self):
        add = ADD4(1, 1, 1, 1, 1, 0, 0, 0, 1)
        assert add.get_output() == (1, 0, 0, 0, 1)

    def test_ADD_4_111110010(self):
        add = ADD4(1, 1, 1, 1, 1, 0, 0, 1, 0)
        assert add.get_output() == (1, 0, 0, 1, 0)

    def test_ADD_4_111110100(self):
        add = ADD4(1, 1, 1, 1, 1, 0, 1, 0, 0)
        assert add.get_output() == (1, 0, 1, 0, 0)

    def test_ADD_4_111111000(self):
        add = ADD4(1, 1, 1, 1, 1, 1, 0, 0, 0)
        assert add.get_output() == (1, 1, 0, 0, 0)

    def test_ADD_4_100001111(self):
        add = ADD4(1, 0, 0, 0, 0, 1, 1, 1, 1)
        assert add.get_output() == (1, 0, 0, 0, 0)

    def test_ADD_4_100011111(self):
        add = ADD4(1, 0, 0, 0, 1, 1, 1, 1, 1)
        assert add.get_output() == (1, 0, 0, 0, 1)

    def test_ADD_4_100101111(self):
        add = ADD4(1, 0, 0, 1, 0, 1, 1, 1, 1)
        assert add.get_output() == (1, 0, 0, 1, 0)

    def test_ADD_4_101001111(self):
        add = ADD4(1, 0, 1, 0, 0, 1, 1, 1, 1)
        assert add.get_output() == (1, 0, 1, 0, 0)

    def test_ADD_4_110001111(self):
        add = ADD4(1, 1, 0, 0, 0, 1, 1, 1, 1)
        assert add.get_output() == (1, 1, 0, 0, 0)

    def test_ADD_4_111111111(self):
        add = ADD4(1, 1, 1, 1, 1, 1, 1, 1, 1)
        assert add.get_output() == (1, 1, 1, 1, 1)
