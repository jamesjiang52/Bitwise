import sys
sys.path.insert(0, "../../")
import gate
from XNOR import XNOR


class TestXNOR:
    def test_XNOR_00(self):
        xnor = XNOR(0, 0)
        assert xnor.get_output() == 1

    def test_XNOR_01(self):
        xnor = XNOR(0, 1)
        assert xnor.get_output() == 0

    def test_XNOR_10(self):
        xnor = XNOR(1, 0)
        assert xnor.get_output() == 0

    def test_XNOR_11(self):
        xnor = XNOR(1, 1)
        assert xnor.get_output() == 1
