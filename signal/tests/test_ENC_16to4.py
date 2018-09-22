import sys
sys.path.append("..")
from ENC import ENC16To4


class TestENC16To4:
    def test_ENC_16to4_00000000000000000(self):
        enc = ENC16To4(0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0)
        assert enc.get_output() == (0, 0, 0, 0, 0)

    def test_ENC_16to4_01111111111111111(self):
        enc = ENC16To4(0, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1)
        assert enc.get_output() == (0, 0, 0, 0, 0)

    def test_ENC_16to4_10000000000000000(self):
        enc = ENC16To4(1, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0)
        assert enc.get_output() == (0, 0, 0, 0, 0)

    def test_ENC_16to4_10000000000000001(self):
        enc = ENC16To4(1, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 1)
        assert enc.get_output() == (1, 0, 0, 0, 0)

    def test_ENC_16to4_10000000000000010(self):
        enc = ENC16To4(1, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 1, 0)
        assert enc.get_output() == (1, 0, 0, 0, 1)

    def test_ENC_16to4_10000000000000011(self):
        enc = ENC16To4(1, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 1, 1)
        assert enc.get_output() == (1, 0, 0, 0, 1)

    def test_ENC_16to4_10000000000000100(self):
        enc = ENC16To4(1, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 1, 0, 0)
        assert enc.get_output() == (1, 0, 0, 1, 0)

    def test_ENC_16to4_10000000000000111(self):
        enc = ENC16To4(1, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 1, 1, 1)
        assert enc.get_output() == (1, 0, 0, 1, 0)

    def test_ENC_16to4_10000000000001000(self):
        enc = ENC16To4(1, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 1, 0, 0, 0)
        assert enc.get_output() == (1, 0, 0, 1, 1)

    def test_ENC_16to4_10000000000001111(self):
        enc = ENC16To4(1, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 1, 1, 1, 1)
        assert enc.get_output() == (1, 0, 0, 1, 1)

    def test_ENC_16to4_10000000000010000(self):
        enc = ENC16To4(1, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 1, 0, 0, 0, 0)
        assert enc.get_output() == (1, 0, 1, 0, 0)

    def test_ENC_16to4_10000000000011111(self):
        enc = ENC16To4(1, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 1, 1, 1, 1, 1)
        assert enc.get_output() == (1, 0, 1, 0, 0)

    def test_ENC_16to4_10000000000100000(self):
        enc = ENC16To4(1, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 1, 0, 0, 0, 0, 0)
        assert enc.get_output() == (1, 0, 1, 0, 1)

    def test_ENC_16to4_10000000000111111(self):
        enc = ENC16To4(1, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 1, 1, 1, 1, 1, 1)
        assert enc.get_output() == (1, 0, 1, 0, 1)

    def test_ENC_16to4_10000000001000000(self):
        enc = ENC16To4(1, 0, 0, 0, 0, 0, 0, 0, 0, 0, 1, 0, 0, 0, 0, 0, 0)
        assert enc.get_output() == (1, 0, 1, 1, 0)

    def test_ENC_16to4_10000000001111111(self):
        enc = ENC16To4(1, 0, 0, 0, 0, 0, 0, 0, 0, 0, 1, 1, 1, 1, 1, 1, 1)
        assert enc.get_output() == (1, 0, 1, 1, 0)

    def test_ENC_16to4_10000000010000000(self):
        enc = ENC16To4(1, 0, 0, 0, 0, 0, 0, 0, 0, 1, 0, 0, 0, 0, 0, 0, 0)
        assert enc.get_output() == (1, 0, 1, 1, 1)

    def test_ENC_16to4_10000000011111111(self):
        enc = ENC16To4(1, 0, 0, 0, 0, 0, 0, 0, 0, 1, 1, 1, 1, 1, 1, 1, 1)
        assert enc.get_output() == (1, 0, 1, 1, 1)

    def test_ENC_16to4_10000000100000000(self):
        enc = ENC16To4(1, 0, 0, 0, 0, 0, 0, 0, 1, 0, 0, 0, 0, 0, 0, 0, 0)
        assert enc.get_output() == (1, 1, 0, 0, 0)

    def test_ENC_16to4_10000000111111111(self):
        enc = ENC16To4(1, 0, 0, 0, 0, 0, 0, 0, 1, 1, 1, 1, 1, 1, 1, 1, 1)
        assert enc.get_output() == (1, 1, 0, 0, 0)

    def test_ENC_16to4_10000001000000000(self):
        enc = ENC16To4(1, 0, 0, 0, 0, 0, 0, 1, 0, 0, 0, 0, 0, 0, 0, 0, 0)
        assert enc.get_output() == (1, 1, 0, 0, 1)

    def test_ENC_16to4_10000001111111111(self):
        enc = ENC16To4(1, 0, 0, 0, 0, 0, 0, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1)
        assert enc.get_output() == (1, 1, 0, 0, 1)

    def test_ENC_16to4_10000010000000000(self):
        enc = ENC16To4(1, 0, 0, 0, 0, 0, 1, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0)
        assert enc.get_output() == (1, 1, 0, 1, 0)

    def test_ENC_16to4_10000011111111111(self):
        enc = ENC16To4(1, 0, 0, 0, 0, 0, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1)
        assert enc.get_output() == (1, 1, 0, 1, 0)

    def test_ENC_16to4_10000100000000000(self):
        enc = ENC16To4(1, 0, 0, 0, 0, 1, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0)
        assert enc.get_output() == (1, 1, 0, 1, 1)

    def test_ENC_16to4_10000111111111111(self):
        enc = ENC16To4(1, 0, 0, 0, 0, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1)
        assert enc.get_output() == (1, 1, 0, 1, 1)

    def test_ENC_16to4_10001000000000000(self):
        enc = ENC16To4(1, 0, 0, 0, 1, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0)
        assert enc.get_output() == (1, 1, 1, 0, 0)

    def test_ENC_16to4_10001111111111111(self):
        enc = ENC16To4(1, 0, 0, 0, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1)
        assert enc.get_output() == (1, 1, 1, 0, 0)

    def test_ENC_16to4_10010000000000000(self):
        enc = ENC16To4(1, 0, 0, 1, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0)
        assert enc.get_output() == (1, 1, 1, 0, 1)

    def test_ENC_16to4_10011111111111111(self):
        enc = ENC16To4(1, 0, 0, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1)
        assert enc.get_output() == (1, 1, 1, 0, 1)

    def test_ENC_16to4_10100000000000000(self):
        enc = ENC16To4(1, 0, 1, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0)
        assert enc.get_output() == (1, 1, 1, 1, 0)

    def test_ENC_16to4_10111111111111111(self):
        enc = ENC16To4(1, 0, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1)
        assert enc.get_output() == (1, 1, 1, 1, 0)

    def test_ENC_16to4_11000000000000000(self):
        enc = ENC16To4(1, 1, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0)
        assert enc.get_output() == (1, 1, 1, 1, 1)

    def test_ENC_16to4_11111111111111111(self):
        enc = ENC16To4(1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1)
        assert enc.get_output() == (1, 1, 1, 1, 1)
