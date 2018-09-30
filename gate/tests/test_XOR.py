import sys
sys.path.insert(0, "../../")
import gate
from XOR import XOR


class TestXOR:
    def test_XOR_00(self):
        xor = XOR(0, 0)
        assert xor.get_output() == 0

    def test_XOR_01(self):
        xor = XOR(0, 1)
        assert xor.get_output() == 1

    def test_XOR_10(self):
        xor = XOR(1, 0)
        assert xor.get_output() == 1

    def test_XOR_11(self):
        xor = XOR(1, 1)
        assert xor.get_output() == 0
