import sys
sys.path.append("..")
from ENC import ENC8To3


class TestENC8To3:
    def test_ENC_8to3_000000000(self):
        enc = ENC8To3(0, 0, 0, 0, 0, 0, 0, 0, 0)
        assert enc.get_output() == (0, 0, 0, 0)

    def test_ENC_8to3_011111111(self):
        enc = ENC8To3(0, 0, 0, 0, 0, 0, 0, 0, 0)
        assert enc.get_output() == (0, 0, 0, 0)

    def test_ENC_8to3_100000000(self):
        enc = ENC8To3(1, 0, 0, 0, 0, 0, 0, 0, 0)
        assert enc.get_output() == (0, 0, 0, 0)

    def test_ENC_8to3_100000001(self):
        enc = ENC8To3(1, 0, 0, 0, 0, 0, 0, 0, 1)
        assert enc.get_output() == (1, 0, 0, 0)

    def test_ENC_8to3_100000010(self):
        enc = ENC8To3(1, 0, 0, 0, 0, 0, 0, 1, 0)
        assert enc.get_output() == (1, 0, 0, 1)

    def test_ENC_8to3_100000011(self):
        enc = ENC8To3(1, 0, 0, 0, 0, 0, 0, 1, 1)
        assert enc.get_output() == (1, 0, 0, 1)

    def test_ENC_8to3_100000100(self):
        enc = ENC8To3(1, 0, 0, 0, 0, 0, 1, 0, 0)
        assert enc.get_output() == (1, 0, 1, 0)

    def test_ENC_8to3_100000111(self):
        enc = ENC8To3(1, 0, 0, 0, 0, 0, 1, 1, 1)
        assert enc.get_output() == (1, 0, 1, 0)

    def test_ENC_8to3_100001000(self):
        enc = ENC8To3(1, 0, 0, 0, 0, 1, 0, 0, 0)
        assert enc.get_output() == (1, 0, 1, 1)

    def test_ENC_8to3_100001111(self):
        enc = ENC8To3(1, 0, 0, 0, 0, 1, 1, 1, 1)
        assert enc.get_output() == (1, 0, 1, 1)

    def test_ENC_8to3_100010000(self):
        enc = ENC8To3(1, 0, 0, 0, 1, 0, 0, 0, 0)
        assert enc.get_output() == (1, 1, 0, 0)

    def test_ENC_8to3_100011111(self):
        enc = ENC8To3(1, 0, 0, 0, 1, 1, 1, 1, 1)
        assert enc.get_output() == (1, 1, 0, 0)

    def test_ENC_8to3_100100000(self):
        enc = ENC8To3(1, 0, 0, 1, 0, 0, 0, 0, 0)
        assert enc.get_output() == (1, 1, 0, 1)

    def test_ENC_8to3_100111111(self):
        enc = ENC8To3(1, 0, 0, 1, 1, 1, 1, 1, 1)
        assert enc.get_output() == (1, 1, 0, 1)

    def test_ENC_8to3_101000000(self):
        enc = ENC8To3(1, 0, 1, 0, 0, 0, 0, 0, 0)
        assert enc.get_output() == (1, 1, 1, 0)

    def test_ENC_8to3_101111111(self):
        enc = ENC8To3(1, 0, 1, 1, 1, 1, 1, 1, 1)
        assert enc.get_output() == (1, 1, 1, 0)

    def test_ENC_8to3_110000000(self):
        enc = ENC8To3(1, 1, 0, 0, 0, 0, 0, 0, 0)
        assert enc.get_output() == (1, 1, 1, 1)

    def test_ENC_8to3_111111111(self):
        enc = ENC8To3(1, 1, 1, 1, 1, 1, 1, 1, 1)
        assert enc.get_output() == (1, 1, 1, 1)
