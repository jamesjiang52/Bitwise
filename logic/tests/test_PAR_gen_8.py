import sys
sys.path.insert(0, "../../")
import logic
from PAR import ParityGenerator8


class TestParityGenerator8:
    def test_PAR_gen_8_00000000(self):
        par_gen = ParityGenerator8(0, 0, 0, 0, 0, 0, 0, 0)
        assert par_gen.get_output() == 0

    def test_PAR_gen_8_00000001(self):
        par_gen = ParityGenerator8(0, 0, 0, 0, 0, 0, 0, 1)
        assert par_gen.get_output() == 1

    def test_PAR_gen_8_00000010(self):
        par_gen = ParityGenerator8(0, 0, 0, 0, 0, 0, 1, 0)
        assert par_gen.get_output() == 1

    def test_PAR_gen_8_00000100(self):
        par_gen = ParityGenerator8(0, 0, 0, 0, 0, 1, 0, 0)
        assert par_gen.get_output() == 1

    def test_PAR_gen_8_00001000(self):
        par_gen = ParityGenerator8(0, 0, 0, 0, 1, 0, 0, 0)
        assert par_gen.get_output() == 1

    def test_PAR_gen_8_00010000(self):
        par_gen = ParityGenerator8(0, 0, 0, 1, 0, 0, 0, 0)
        assert par_gen.get_output() == 1

    def test_PAR_gen_8_00100000(self):
        par_gen = ParityGenerator8(0, 0, 1, 0, 0, 0, 0, 0)
        assert par_gen.get_output() == 1

    def test_PAR_gen_8_01000000(self):
        par_gen = ParityGenerator8(0, 1, 0, 0, 0, 0, 0, 0)
        assert par_gen.get_output() == 1

    def test_PAR_gen_8_10000000(self):
        par_gen = ParityGenerator8(1, 0, 0, 0, 0, 0, 0, 0)
        assert par_gen.get_output() == 1

    def test_PAR_gen_8_00000011(self):
        par_gen = ParityGenerator8(0, 0, 0, 0, 0, 0, 1, 1)
        assert par_gen.get_output() == 0

    def test_PAR_gen_8_00000110(self):
        par_gen = ParityGenerator8(0, 0, 0, 0, 0, 1, 1, 0)
        assert par_gen.get_output() == 0

    def test_PAR_gen_8_00001100(self):
        par_gen = ParityGenerator8(0, 0, 0, 0, 1, 1, 0, 0)
        assert par_gen.get_output() == 0

    def test_PAR_gen_8_00011000(self):
        par_gen = ParityGenerator8(0, 0, 0, 1, 1, 0, 0, 0)
        assert par_gen.get_output() == 0

    def test_PAR_gen_8_00110000(self):
        par_gen = ParityGenerator8(0, 0, 1, 1, 0, 0, 0, 0)
        assert par_gen.get_output() == 0

    def test_PAR_gen_8_01100000(self):
        par_gen = ParityGenerator8(0, 1, 1, 0, 0, 0, 0, 0)
        assert par_gen.get_output() == 0

    def test_PAR_gen_8_11000000(self):
        par_gen = ParityGenerator8(1, 1, 0, 0, 0, 0, 0, 0)
        assert par_gen.get_output() == 0

    def test_PAR_gen_8_00000111(self):
        par_gen = ParityGenerator8(0, 0, 0, 0, 0, 1, 1, 1)
        assert par_gen.get_output() == 1

    def test_PAR_gen_8_00001110(self):
        par_gen = ParityGenerator8(0, 0, 0, 0, 1, 1, 1, 0)
        assert par_gen.get_output() == 1

    def test_PAR_gen_8_00011100(self):
        par_gen = ParityGenerator8(0, 0, 0, 1, 1, 1, 0, 0)
        assert par_gen.get_output() == 1

    def test_PAR_gen_8_00111000(self):
        par_gen = ParityGenerator8(0, 0, 1, 1, 1, 0, 0, 0)
        assert par_gen.get_output() == 1

    def test_PAR_gen_8_01110000(self):
        par_gen = ParityGenerator8(0, 1, 1, 1, 0, 0, 0, 0)
        assert par_gen.get_output() == 1

    def test_PAR_gen_8_11100000(self):
        par_gen = ParityGenerator8(1, 1, 1, 0, 0, 0, 0, 0)
        assert par_gen.get_output() == 1

    def test_PAR_gen_8_00001111(self):
        par_gen = ParityGenerator8(0, 0, 0, 0, 1, 1, 1, 1)
        assert par_gen.get_output() == 0

    def test_PAR_gen_8_00011110(self):
        par_gen = ParityGenerator8(0, 0, 0, 1, 1, 1, 1, 0)
        assert par_gen.get_output() == 0

    def test_PAR_gen_8_00111100(self):
        par_gen = ParityGenerator8(0, 0, 1, 1, 1, 1, 0, 0)
        assert par_gen.get_output() == 0

    def test_PAR_gen_8_01111000(self):
        par_gen = ParityGenerator8(0, 1, 1, 1, 1, 0, 0, 0)
        assert par_gen.get_output() == 0

    def test_PAR_gen_8_11110000(self):
        par_gen = ParityGenerator8(1, 1, 1, 1, 0, 0, 0, 0)
        assert par_gen.get_output() == 0

    def test_PAR_gen_8_00011111(self):
        par_gen = ParityGenerator8(0, 0, 0, 1, 1, 1, 1, 1)
        assert par_gen.get_output() == 1

    def test_PAR_gen_8_00111110(self):
        par_gen = ParityGenerator8(0, 0, 1, 1, 1, 1, 1, 0)
        assert par_gen.get_output() == 1

    def test_PAR_gen_8_01111100(self):
        par_gen = ParityGenerator8(0, 1, 1, 1, 1, 1, 0, 0)
        assert par_gen.get_output() == 1

    def test_PAR_gen_8_11111000(self):
        par_gen = ParityGenerator8(1, 1, 1, 1, 1, 0, 0, 0)
        assert par_gen.get_output() == 1

    def test_PAR_gen_8_00111111(self):
        par_gen = ParityGenerator8(0, 0, 1, 1, 1, 1, 1, 1)
        assert par_gen.get_output() == 0

    def test_PAR_gen_8_01111110(self):
        par_gen = ParityGenerator8(0, 1, 1, 1, 1, 1, 1, 0)
        assert par_gen.get_output() == 0

    def test_PAR_gen_8_11111100(self):
        par_gen = ParityGenerator8(1, 1, 1, 1, 1, 1, 0, 0)
        assert par_gen.get_output() == 0

    def test_PAR_gen_8_01111111(self):
        par_gen = ParityGenerator8(0, 1, 1, 1, 1, 1, 1, 1)
        assert par_gen.get_output() == 1

    def test_PAR_gen_8_11111110(self):
        par_gen = ParityGenerator8(1, 1, 1, 1, 1, 1, 1, 0)
        assert par_gen.get_output() == 1

    def test_PAR_gen_8_11111111(self):
        par_gen = ParityGenerator8(1, 1, 1, 1, 1, 1, 1, 1)
        assert par_gen.get_output() == 0
