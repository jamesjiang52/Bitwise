import sys
sys.path.append("../")
import XOR


class TestXOR:
    def test_XOR_00(self):
        _xor = XOR.XOR(0, 0)
        assert _xor.get_output() == 0

    def test_XOR_01(self):
        _xor = XOR.XOR(0, 1)
        assert _xor.get_output() == 1

    def test_XOR_10(self):
        _xor = XOR.XOR(1, 0)
        assert _xor.get_output() == 1

    def test_XOR_11(self):
        _xor = XOR.XOR(1, 1)
        assert _xor.get_output() == 0
