import sys
sys.path.append("..")
from ENC import ENC4To2


class TestENC4To2:
    def test_ENC_4to2_00000(self):
        enc = ENC4To2(0, 0, 0, 0, 0)
        assert enc.get_output() == (0, 0, 0)

    def test_ENC_4to2_01111(self):
        enc = ENC4To2(0, 1, 1, 1, 1)
        assert enc.get_output() == (0, 0, 0)

    def test_ENC_4to2_10000(self):
        enc = ENC4To2(1, 0, 0, 0, 0)
        assert enc.get_output() == (0, 0, 0)

    def test_ENC_4to2_10001(self):
        enc = ENC4To2(1, 0, 0, 0, 1)
        assert enc.get_output() == (1, 0, 0)

    def test_ENC_4to2_10010(self):
        enc = ENC4To2(1, 0, 0, 1, 0)
        assert enc.get_output() == (1, 0, 1)

    def test_ENC_4to2_10011(self):
        enc = ENC4To2(1, 0, 0, 1, 1)
        assert enc.get_output() == (1, 0, 1)

    def test_ENC_4to2_10100(self):
        enc = ENC4To2(1, 0, 1, 0, 0)
        assert enc.get_output() == (1, 1, 0)

    def test_ENC_4to2_10111(self):
        enc = ENC4To2(1, 0, 1, 1, 1)
        assert enc.get_output() == (1, 1, 0)

    def test_ENC_4to2_11000(self):
        enc = ENC4To2(1, 1, 0, 0, 0)
        assert enc.get_output() == (1, 1, 1)

    def test_ENC_4to2_11111(self):
        enc = ENC4To2(1, 1, 1, 1, 1)
        assert enc.get_output() == (1, 1, 1)
