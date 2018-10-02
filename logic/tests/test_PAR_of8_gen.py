import sys
sys.path.insert(0, "../../")
import logic
from PAR import PAROf8Gen


class TestPAROf8Gen:
    def test_PAR_of8_gen_00000000(self):
        par_gen = PAROf8Gen(0, 0, 0, 0, 0, 0, 0, 0)
        assert par_gen.get_output() == 0

    def test_PAR_of8_gen_00000001(self):
        par_gen = PAROf8Gen(0, 0, 0, 0, 0, 0, 0, 1)
        assert par_gen.get_output() == 1

    def test_PAR_of8_gen_00000010(self):
        par_gen = PAROf8Gen(0, 0, 0, 0, 0, 0, 1, 0)
        assert par_gen.get_output() == 1

    def test_PAR_of8_gen_00000100(self):
        par_gen = PAROf8Gen(0, 0, 0, 0, 0, 1, 0, 0)
        assert par_gen.get_output() == 1

    def test_PAR_of8_gen_00001000(self):
        par_gen = PAROf8Gen(0, 0, 0, 0, 1, 0, 0, 0)
        assert par_gen.get_output() == 1

    def test_PAR_of8_gen_00010000(self):
        par_gen = PAROf8Gen(0, 0, 0, 1, 0, 0, 0, 0)
        assert par_gen.get_output() == 1

    def test_PAR_of8_gen_00100000(self):
        par_gen = PAROf8Gen(0, 0, 1, 0, 0, 0, 0, 0)
        assert par_gen.get_output() == 1

    def test_PAR_of8_gen_01000000(self):
        par_gen = PAROf8Gen(0, 1, 0, 0, 0, 0, 0, 0)
        assert par_gen.get_output() == 1

    def test_PAR_of8_gen_10000000(self):
        par_gen = PAROf8Gen(1, 0, 0, 0, 0, 0, 0, 0)
        assert par_gen.get_output() == 1

    def test_PAR_of8_gen_00000011(self):
        par_gen = PAROf8Gen(0, 0, 0, 0, 0, 0, 1, 1)
        assert par_gen.get_output() == 0

    def test_PAR_of8_gen_00000110(self):
        par_gen = PAROf8Gen(0, 0, 0, 0, 0, 1, 1, 0)
        assert par_gen.get_output() == 0

    def test_PAR_of8_gen_00001100(self):
        par_gen = PAROf8Gen(0, 0, 0, 0, 1, 1, 0, 0)
        assert par_gen.get_output() == 0

    def test_PAR_of8_gen_00011000(self):
        par_gen = PAROf8Gen(0, 0, 0, 1, 1, 0, 0, 0)
        assert par_gen.get_output() == 0

    def test_PAR_of8_gen_00110000(self):
        par_gen = PAROf8Gen(0, 0, 1, 1, 0, 0, 0, 0)
        assert par_gen.get_output() == 0

    def test_PAR_of8_gen_01100000(self):
        par_gen = PAROf8Gen(0, 1, 1, 0, 0, 0, 0, 0)
        assert par_gen.get_output() == 0

    def test_PAR_of8_gen_11000000(self):
        par_gen = PAROf8Gen(1, 1, 0, 0, 0, 0, 0, 0)
        assert par_gen.get_output() == 0

    def test_PAR_of8_gen_00000111(self):
        par_gen = PAROf8Gen(0, 0, 0, 0, 0, 1, 1, 1)
        assert par_gen.get_output() == 1

    def test_PAR_of8_gen_00001110(self):
        par_gen = PAROf8Gen(0, 0, 0, 0, 1, 1, 1, 0)
        assert par_gen.get_output() == 1

    def test_PAR_of8_gen_00011100(self):
        par_gen = PAROf8Gen(0, 0, 0, 1, 1, 1, 0, 0)
        assert par_gen.get_output() == 1

    def test_PAR_of8_gen_00111000(self):
        par_gen = PAROf8Gen(0, 0, 1, 1, 1, 0, 0, 0)
        assert par_gen.get_output() == 1

    def test_PAR_of8_gen_01110000(self):
        par_gen = PAROf8Gen(0, 1, 1, 1, 0, 0, 0, 0)
        assert par_gen.get_output() == 1

    def test_PAR_of8_gen_11100000(self):
        par_gen = PAROf8Gen(1, 1, 1, 0, 0, 0, 0, 0)
        assert par_gen.get_output() == 1

    def test_PAR_of8_gen_00001111(self):
        par_gen = PAROf8Gen(0, 0, 0, 0, 1, 1, 1, 1)
        assert par_gen.get_output() == 0

    def test_PAR_of8_gen_00011110(self):
        par_gen = PAROf8Gen(0, 0, 0, 1, 1, 1, 1, 0)
        assert par_gen.get_output() == 0

    def test_PAR_of8_gen_00111100(self):
        par_gen = PAROf8Gen(0, 0, 1, 1, 1, 1, 0, 0)
        assert par_gen.get_output() == 0

    def test_PAR_of8_gen_01111000(self):
        par_gen = PAROf8Gen(0, 1, 1, 1, 1, 0, 0, 0)
        assert par_gen.get_output() == 0

    def test_PAR_of8_gen_11110000(self):
        par_gen = PAROf8Gen(1, 1, 1, 1, 0, 0, 0, 0)
        assert par_gen.get_output() == 0

    def test_PAR_of8_gen_00011111(self):
        par_gen = PAROf8Gen(0, 0, 0, 1, 1, 1, 1, 1)
        assert par_gen.get_output() == 1

    def test_PAR_of8_gen_00111110(self):
        par_gen = PAROf8Gen(0, 0, 1, 1, 1, 1, 1, 0)
        assert par_gen.get_output() == 1

    def test_PAR_of8_gen_01111100(self):
        par_gen = PAROf8Gen(0, 1, 1, 1, 1, 1, 0, 0)
        assert par_gen.get_output() == 1

    def test_PAR_of8_gen_11111000(self):
        par_gen = PAROf8Gen(1, 1, 1, 1, 1, 0, 0, 0)
        assert par_gen.get_output() == 1

    def test_PAR_of8_gen_00111111(self):
        par_gen = PAROf8Gen(0, 0, 1, 1, 1, 1, 1, 1)
        assert par_gen.get_output() == 0

    def test_PAR_of8_gen_01111110(self):
        par_gen = PAROf8Gen(0, 1, 1, 1, 1, 1, 1, 0)
        assert par_gen.get_output() == 0

    def test_PAR_of8_gen_11111100(self):
        par_gen = PAROf8Gen(1, 1, 1, 1, 1, 1, 0, 0)
        assert par_gen.get_output() == 0

    def test_PAR_of8_gen_01111111(self):
        par_gen = PAROf8Gen(0, 1, 1, 1, 1, 1, 1, 1)
        assert par_gen.get_output() == 1

    def test_PAR_of8_gen_11111110(self):
        par_gen = PAROf8Gen(1, 1, 1, 1, 1, 1, 1, 0)
        assert par_gen.get_output() == 1

    def test_PAR_of8_gen_11111111(self):
        par_gen = PAROf8Gen(1, 1, 1, 1, 1, 1, 1, 1)
        assert par_gen.get_output() == 0
