import sys
sys.path.append("../")
import XNOR


class TestXNOR:
    def test_XNOR_00(self):
        _xnor = XNOR.XNOR(0, 0)
        assert _xnor.get_output() == 1

    def test_XNOR_01(self):
        _xnor = XNOR.XNOR(0, 1)
        assert _xnor.get_output() == 0

    def test_XNOR_10(self):
        _xnor = XNOR.XNOR(1, 0)
        assert _xnor.get_output() == 0

    def test_XNOR_11(self):
        _xnor = XNOR.XNOR(1, 1)
        assert _xnor.get_output() == 1
