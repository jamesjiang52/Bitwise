import sys
sys.path.insert(0, "../../")
import arithmetic
from ADD import ADD4


class TestADD4:
    def test_ADD_4_00000000(self):
        add = ADD4(0, 0, 0, 0, 0, 0, 0, 0)
        assert add.get_output() == (0, 0, 0, 0, 0)

    def test_ADD_4_00010001(self):
        add = ADD4(0, 0, 0, 1, 0, 0, 0, 1)
        assert add.get_output() == (0, 0, 0, 1, 0)

    def test_ADD_4_00100010(self):
        add = ADD4(0, 0, 1, 0, 0, 0, 1, 0)
        assert add.get_output() == (0, 0, 1, 0, 0)

    def test_ADD_4_01000100(self):
        add = ADD4(0, 1, 0, 0, 0, 1, 0, 0)
        assert add.get_output() == (0, 1, 0, 0, 0)

    def test_ADD_4_10001000(self):
        add = ADD4(1, 0, 0, 0, 1, 0, 0, 0)
        assert add.get_output() == (1, 0, 0, 0, 0)

    def test_ADD_4_00000001(self):
        add = ADD4(0, 0, 0, 0, 0, 0, 0, 1)
        assert add.get_output() == (0, 0, 0, 0, 1)

    def test_ADD_4_00000010(self):
        add = ADD4(0, 0, 0, 0, 0, 0, 1, 0)
        assert add.get_output() == (0, 0, 0, 1, 0)

    def test_ADD_4_00000100(self):
        add = ADD4(0, 0, 0, 0, 0, 1, 0, 0)
        assert add.get_output() == (0, 0, 1, 0, 0)

    def test_ADD_4_00001000(self):
        add = ADD4(0, 0, 0, 0, 1, 0, 0, 0)
        assert add.get_output() == (0, 1, 0, 0, 0)

    def test_ADD_4_00010000(self):
        add = ADD4(0, 0, 0, 1, 0, 0, 0, 0)
        assert add.get_output() == (0, 0, 0, 0, 1)

    def test_ADD_4_00100000(self):
        add = ADD4(0, 0, 1, 0, 0, 0, 0, 0)
        assert add.get_output() == (0, 0, 0, 1, 0)

    def test_ADD_4_01000000(self):
        add = ADD4(0, 1, 0, 0, 0, 0, 0, 0)
        assert add.get_output() == (0, 0, 1, 0, 0)

    def test_ADD_4_10000000(self):
        add = ADD4(1, 0, 0, 0, 0, 0, 0, 0)
        assert add.get_output() == (0, 1, 0, 0, 0)

    def test_ADD_4_11110000(self):
        add = ADD4(1, 1, 1, 1, 0, 0, 0, 0)
        assert add.get_output() == (0, 1, 1, 1, 1)

    def test_ADD_4_11110001(self):
        add = ADD4(1, 1, 1, 1, 0, 0, 0, 1)
        assert add.get_output() == (1, 0, 0, 0, 0)

    def test_ADD_4_11110010(self):
        add = ADD4(1, 1, 1, 1, 0, 0, 1, 0)
        assert add.get_output() == (1, 0, 0, 0, 1)

    def test_ADD_4_11110100(self):
        add = ADD4(1, 1, 1, 1, 0, 1, 0, 0)
        assert add.get_output() == (1, 0, 0, 1, 1)

    def test_ADD_4_11111000(self):
        add = ADD4(1, 1, 1, 1, 1, 0, 0, 0)
        assert add.get_output() == (1, 0, 1, 1, 1)

    def test_ADD_4_00001111(self):
        add = ADD4(0, 0, 0, 0, 1, 1, 1, 1)
        assert add.get_output() == (0, 1, 1, 1, 1)

    def test_ADD_4_00011111(self):
        add = ADD4(0, 0, 0, 1, 1, 1, 1, 1)
        assert add.get_output() == (1, 0, 0, 0, 0)

    def test_ADD_4_00101111(self):
        add = ADD4(0, 0, 1, 0, 1, 1, 1, 1)
        assert add.get_output() == (1, 0, 0, 0, 1)

    def test_ADD_4_01001111(self):
        add = ADD4(0, 1, 0, 0, 1, 1, 1, 1)
        assert add.get_output() == (1, 0, 0, 1, 1)

    def test_ADD_4_10001111(self):
        add = ADD4(1, 0, 0, 0, 1, 1, 1, 1)
        assert add.get_output() == (1, 0, 1, 1, 1)

    def test_ADD_4_11111111(self):
        add = ADD4(1, 1, 1, 1, 1, 1, 1, 1)
        assert add.get_output() == (1, 1, 1, 1, 0)
