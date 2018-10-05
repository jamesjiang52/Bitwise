import sys
sys.path.insert(0, "../../")
import logic
from PAR import ParityGenerator16


class TestParityGenerator16:
    def test_PAR_gen_16_0000000000000000(self):
        par_gen = ParityGenerator16(
            0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0)
        assert par_gen.get_output() == 0

    def test_PAR_gen_16_0000000000000001(self):
        par_gen = ParityGenerator16(
            0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 1)
        assert par_gen.get_output() == 1

    def test_PAR_gen_16_0000000000000010(self):
        par_gen = ParityGenerator16(
            0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 1, 0)
        assert par_gen.get_output() == 1

    def test_PAR_gen_16_0000000000000100(self):
        par_gen = ParityGenerator16(
            0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 1, 0, 0)
        assert par_gen.get_output() == 1

    def test_PAR_gen_16_0000000000001000(self):
        par_gen = ParityGenerator16(
            0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 1, 0, 0, 0)
        assert par_gen.get_output() == 1

    def test_PAR_gen_16_0000000000010000(self):
        par_gen = ParityGenerator16(
            0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 1, 0, 0, 0, 0)
        assert par_gen.get_output() == 1

    def test_PAR_gen_16_0000000000100000(self):
        par_gen = ParityGenerator16(
            0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 1, 0, 0, 0, 0, 0)
        assert par_gen.get_output() == 1

    def test_PAR_gen_16_0000000001000000(self):
        par_gen = ParityGenerator16(
            0, 0, 0, 0, 0, 0, 0, 0, 0, 1, 0, 0, 0, 0, 0, 0)
        assert par_gen.get_output() == 1

    def test_PAR_gen_16_0000000010000000(self):
        par_gen = ParityGenerator16(
            0, 0, 0, 0, 0, 0, 0, 0, 1, 0, 0, 0, 0, 0, 0, 0)
        assert par_gen.get_output() == 1

    def test_PAR_gen_16_0000000100000000(self):
        par_gen = ParityGenerator16(
            0, 0, 0, 0, 0, 0, 0, 1, 0, 0, 0, 0, 0, 0, 0, 0)
        assert par_gen.get_output() == 1

    def test_PAR_gen_16_0000001000000000(self):
        par_gen = ParityGenerator16(
            0, 0, 0, 0, 0, 0, 1, 0, 0, 0, 0, 0, 0, 0, 0, 0)
        assert par_gen.get_output() == 1

    def test_PAR_gen_16_0000010000000000(self):
        par_gen = ParityGenerator16(
            0, 0, 0, 0, 0, 1, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0)
        assert par_gen.get_output() == 1

    def test_PAR_gen_16_0000100000000000(self):
        par_gen = ParityGenerator16(
            0, 0, 0, 0, 1, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0)
        assert par_gen.get_output() == 1

    def test_PAR_gen_16_0001000000000000(self):
        par_gen = ParityGenerator16(
            0, 0, 0, 1, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0)
        assert par_gen.get_output() == 1

    def test_PAR_gen_16_0010000000000000(self):
        par_gen = ParityGenerator16(
            0, 0, 1, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0)
        assert par_gen.get_output() == 1

    def test_PAR_gen_16_0100000000000000(self):
        par_gen = ParityGenerator16(
            0, 1, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0)
        assert par_gen.get_output() == 1

    def test_PAR_gen_16_1000000000000000(self):
        par_gen = ParityGenerator16(
            1, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0)
        assert par_gen.get_output() == 1

    def test_PAR_gen_16_0000000000000011(self):
        par_gen = ParityGenerator16(
            0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 1, 1)
        assert par_gen.get_output() == 0

    def test_PAR_gen_16_0000000000000110(self):
        par_gen = ParityGenerator16(
            0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 1, 1, 0)
        assert par_gen.get_output() == 0

    def test_PAR_gen_16_0000000000001100(self):
        par_gen = ParityGenerator16(
            0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 1, 1, 0, 0)
        assert par_gen.get_output() == 0

    def test_PAR_gen_16_0000000000011000(self):
        par_gen = ParityGenerator16(
            0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 1, 1, 0, 0, 0)
        assert par_gen.get_output() == 0

    def test_PAR_gen_16_0000000000110000(self):
        par_gen = ParityGenerator16(
            0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 1, 1, 0, 0, 0, 0)
        assert par_gen.get_output() == 0

    def test_PAR_gen_16_0000000001100000(self):
        par_gen = ParityGenerator16(
            0, 0, 0, 0, 0, 0, 0, 0, 0, 1, 1, 0, 0, 0, 0, 0)
        assert par_gen.get_output() == 0

    def test_PAR_gen_16_0000000011000000(self):
        par_gen = ParityGenerator16(
            0, 0, 0, 0, 0, 0, 0, 0, 1, 1, 0, 0, 0, 0, 0, 0)
        assert par_gen.get_output() == 0

    def test_PAR_gen_16_0000000110000000(self):
        par_gen = ParityGenerator16(
            0, 0, 0, 0, 0, 0, 0, 1, 1, 0, 0, 0, 0, 0, 0, 0)
        assert par_gen.get_output() == 0

    def test_PAR_gen_16_0000001100000000(self):
        par_gen = ParityGenerator16(
            0, 0, 0, 0, 0, 0, 1, 1, 0, 0, 0, 0, 0, 0, 0, 0)
        assert par_gen.get_output() == 0

    def test_PAR_gen_16_0000011000000000(self):
        par_gen = ParityGenerator16(
            0, 0, 0, 0, 0, 1, 1, 0, 0, 0, 0, 0, 0, 0, 0, 0)
        assert par_gen.get_output() == 0

    def test_PAR_gen_16_0000110000000000(self):
        par_gen = ParityGenerator16(
            0, 0, 0, 0, 1, 1, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0)
        assert par_gen.get_output() == 0

    def test_PAR_gen_16_0001100000000000(self):
        par_gen = ParityGenerator16(
            0, 0, 0, 1, 1, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0)
        assert par_gen.get_output() == 0

    def test_PAR_gen_16_0011000000000000(self):
        par_gen = ParityGenerator16(
            0, 0, 1, 1, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0)
        assert par_gen.get_output() == 0

    def test_PAR_gen_16_0110000000000000(self):
        par_gen = ParityGenerator16(
            0, 1, 1, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0)
        assert par_gen.get_output() == 0

    def test_PAR_gen_16_1100000000000000(self):
        par_gen = ParityGenerator16(
            1, 1, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0)
        assert par_gen.get_output() == 0

    def test_PAR_gen_16_0000000000000111(self):
        par_gen = ParityGenerator16(
            0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 1, 1, 1)
        assert par_gen.get_output() == 1

    def test_PAR_gen_16_0000000000001110(self):
        par_gen = ParityGenerator16(
            0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 1, 1, 1, 0)
        assert par_gen.get_output() == 1

    def test_PAR_gen_16_0000000000011100(self):
        par_gen = ParityGenerator16(
            0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 1, 1, 1, 0, 0)
        assert par_gen.get_output() == 1

    def test_PAR_gen_16_0000000000111000(self):
        par_gen = ParityGenerator16(
            0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 1, 1, 1, 0, 0, 0)
        assert par_gen.get_output() == 1

    def test_PAR_gen_16_0000000001110000(self):
        par_gen = ParityGenerator16(
            0, 0, 0, 0, 0, 0, 0, 0, 0, 1, 1, 1, 0, 0, 0, 0)
        assert par_gen.get_output() == 1

    def test_PAR_gen_16_0000000011100000(self):
        par_gen = ParityGenerator16(
            0, 0, 0, 0, 0, 0, 0, 0, 1, 1, 1, 0, 0, 0, 0, 0)
        assert par_gen.get_output() == 1

    def test_PAR_gen_16_0000000111000000(self):
        par_gen = ParityGenerator16(
            0, 0, 0, 0, 0, 0, 0, 1, 1, 1, 0, 0, 0, 0, 0, 0)
        assert par_gen.get_output() == 1

    def test_PAR_gen_16_0000001110000000(self):
        par_gen = ParityGenerator16(
            0, 0, 0, 0, 0, 0, 1, 1, 1, 0, 0, 0, 0, 0, 0, 0)
        assert par_gen.get_output() == 1

    def test_PAR_gen_16_0000011100000000(self):
        par_gen = ParityGenerator16(
            0, 0, 0, 0, 0, 1, 1, 1, 0, 0, 0, 0, 0, 0, 0, 0)
        assert par_gen.get_output() == 1

    def test_PAR_gen_16_0000111000000000(self):
        par_gen = ParityGenerator16(
            0, 0, 0, 0, 1, 1, 1, 0, 0, 0, 0, 0, 0, 0, 0, 0)
        assert par_gen.get_output() == 1

    def test_PAR_gen_16_0001110000000000(self):
        par_gen = ParityGenerator16(
            0, 0, 0, 1, 1, 1, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0)
        assert par_gen.get_output() == 1

    def test_PAR_gen_16_0011100000000000(self):
        par_gen = ParityGenerator16(
            0, 0, 1, 1, 1, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0)
        assert par_gen.get_output() == 1

    def test_PAR_gen_16_0111000000000000(self):
        par_gen = ParityGenerator16(
            0, 1, 1, 1, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0)
        assert par_gen.get_output() == 1

    def test_PAR_gen_16_1110000000000000(self):
        par_gen = ParityGenerator16(
            1, 1, 1, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0)
        assert par_gen.get_output() == 1

    def test_PAR_gen_16_0000000000001111(self):
        par_gen = ParityGenerator16(
            0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 1, 1, 1, 1)
        assert par_gen.get_output() == 0

    def test_PAR_gen_16_0000000000011110(self):
        par_gen = ParityGenerator16(
            0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 1, 1, 1, 1, 0)
        assert par_gen.get_output() == 0

    def test_PAR_gen_16_0000000000111100(self):
        par_gen = ParityGenerator16(
            0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 1, 1, 1, 1, 0, 0)
        assert par_gen.get_output() == 0

    def test_PAR_gen_16_0000000001111000(self):
        par_gen = ParityGenerator16(
            0, 0, 0, 0, 0, 0, 0, 0, 0, 1, 1, 1, 1, 0, 0, 0)
        assert par_gen.get_output() == 0

    def test_PAR_gen_16_0000000011110000(self):
        par_gen = ParityGenerator16(
            0, 0, 0, 0, 0, 0, 0, 0, 1, 1, 1, 1, 0, 0, 0, 0)
        assert par_gen.get_output() == 0

    def test_PAR_gen_16_0000000111100000(self):
        par_gen = ParityGenerator16(
            0, 0, 0, 0, 0, 0, 0, 1, 1, 1, 1, 0, 0, 0, 0, 0)
        assert par_gen.get_output() == 0

    def test_PAR_gen_16_0000001111000000(self):
        par_gen = ParityGenerator16(
            0, 0, 0, 0, 0, 0, 1, 1, 1, 1, 0, 0, 0, 0, 0, 0)
        assert par_gen.get_output() == 0

    def test_PAR_gen_16_0000011110000000(self):
        par_gen = ParityGenerator16(
            0, 0, 0, 0, 0, 1, 1, 1, 1, 0, 0, 0, 0, 0, 0, 0)
        assert par_gen.get_output() == 0

    def test_PAR_gen_16_0000111100000000(self):
        par_gen = ParityGenerator16(
            0, 0, 0, 0, 1, 1, 1, 1, 0, 0, 0, 0, 0, 0, 0, 0)
        assert par_gen.get_output() == 0

    def test_PAR_gen_16_0001111000000000(self):
        par_gen = ParityGenerator16(
            0, 0, 0, 1, 1, 1, 1, 0, 0, 0, 0, 0, 0, 0, 0, 0)
        assert par_gen.get_output() == 0

    def test_PAR_gen_16_0011110000000000(self):
        par_gen = ParityGenerator16(
            0, 0, 1, 1, 1, 1, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0)
        assert par_gen.get_output() == 0

    def test_PAR_gen_16_0111100000000000(self):
        par_gen = ParityGenerator16(
            0, 1, 1, 1, 1, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0)
        assert par_gen.get_output() == 0

    def test_PAR_gen_16_1111000000000000(self):
        par_gen = ParityGenerator16(
            1, 1, 1, 1, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0)
        assert par_gen.get_output() == 0

    def test_PAR_gen_16_0000000000011111(self):
        par_gen = ParityGenerator16(
            0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 1, 1, 1, 1, 1)
        assert par_gen.get_output() == 1

    def test_PAR_gen_16_0000000000111110(self):
        par_gen = ParityGenerator16(
            0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 1, 1, 1, 1, 1, 0)
        assert par_gen.get_output() == 1

    def test_PAR_gen_16_0000000001111100(self):
        par_gen = ParityGenerator16(
            0, 0, 0, 0, 0, 0, 0, 0, 0, 1, 1, 1, 1, 1, 0, 0)
        assert par_gen.get_output() == 1

    def test_PAR_gen_16_0000000011111000(self):
        par_gen = ParityGenerator16(
            0, 0, 0, 0, 0, 0, 0, 0, 1, 1, 1, 1, 1, 0, 0, 0)
        assert par_gen.get_output() == 1

    def test_PAR_gen_16_0000000111110000(self):
        par_gen = ParityGenerator16(
            0, 0, 0, 0, 0, 0, 0, 1, 1, 1, 1, 1, 0, 0, 0, 0)
        assert par_gen.get_output() == 1

    def test_PAR_gen_16_0000001111100000(self):
        par_gen = ParityGenerator16(
            0, 0, 0, 0, 0, 0, 1, 1, 1, 1, 1, 0, 0, 0, 0, 0)
        assert par_gen.get_output() == 1

    def test_PAR_gen_16_0000011111000000(self):
        par_gen = ParityGenerator16(
            0, 0, 0, 0, 0, 1, 1, 1, 1, 1, 0, 0, 0, 0, 0, 0)
        assert par_gen.get_output() == 1

    def test_PAR_gen_16_0000111110000000(self):
        par_gen = ParityGenerator16(
            0, 0, 0, 0, 1, 1, 1, 1, 1, 0, 0, 0, 0, 0, 0, 0)
        assert par_gen.get_output() == 1

    def test_PAR_gen_16_0001111100000000(self):
        par_gen = ParityGenerator16(
            0, 0, 0, 1, 1, 1, 1, 1, 0, 0, 0, 0, 0, 0, 0, 0)
        assert par_gen.get_output() == 1

    def test_PAR_gen_16_0011111000000000(self):
        par_gen = ParityGenerator16(
            0, 0, 1, 1, 1, 1, 1, 0, 0, 0, 0, 0, 0, 0, 0, 0)
        assert par_gen.get_output() == 1

    def test_PAR_gen_16_0111110000000000(self):
        par_gen = ParityGenerator16(
            0, 1, 1, 1, 1, 1, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0)
        assert par_gen.get_output() == 1

    def test_PAR_gen_16_1111100000000000(self):
        par_gen = ParityGenerator16(
            1, 1, 1, 1, 1, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0)
        assert par_gen.get_output() == 1

    def test_PAR_gen_16_0000000000111111(self):
        par_gen = ParityGenerator16(
            0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 1, 1, 1, 1, 1, 1)
        assert par_gen.get_output() == 0

    def test_PAR_gen_16_0000000001111110(self):
        par_gen = ParityGenerator16(
            0, 0, 0, 0, 0, 0, 0, 0, 0, 1, 1, 1, 1, 1, 1, 0)
        assert par_gen.get_output() == 0

    def test_PAR_gen_16_0000000011111100(self):
        par_gen = ParityGenerator16(
            0, 0, 0, 0, 0, 0, 0, 0, 1, 1, 1, 1, 1, 1, 0, 0)
        assert par_gen.get_output() == 0

    def test_PAR_gen_16_0000000111111000(self):
        par_gen = ParityGenerator16(
            0, 0, 0, 0, 0, 0, 0, 1, 1, 1, 1, 1, 1, 0, 0, 0)
        assert par_gen.get_output() == 0

    def test_PAR_gen_16_0000001111110000(self):
        par_gen = ParityGenerator16(
            0, 0, 0, 0, 0, 0, 1, 1, 1, 1, 1, 1, 0, 0, 0, 0)
        assert par_gen.get_output() == 0

    def test_PAR_gen_16_0000011111100000(self):
        par_gen = ParityGenerator16(
            0, 0, 0, 0, 0, 1, 1, 1, 1, 1, 1, 0, 0, 0, 0, 0)
        assert par_gen.get_output() == 0

    def test_PAR_gen_16_0000111111000000(self):
        par_gen = ParityGenerator16(
            0, 0, 0, 0, 1, 1, 1, 1, 1, 1, 0, 0, 0, 0, 0, 0)
        assert par_gen.get_output() == 0

    def test_PAR_gen_16_0001111110000000(self):
        par_gen = ParityGenerator16(
            0, 0, 0, 1, 1, 1, 1, 1, 1, 0, 0, 0, 0, 0, 0, 0)
        assert par_gen.get_output() == 0

    def test_PAR_gen_16_0011111100000000(self):
        par_gen = ParityGenerator16(
            0, 0, 1, 1, 1, 1, 1, 1, 0, 0, 0, 0, 0, 0, 0, 0)
        assert par_gen.get_output() == 0

    def test_PAR_gen_16_0111111000000000(self):
        par_gen = ParityGenerator16(
            0, 1, 1, 1, 1, 1, 1, 0, 0, 0, 0, 0, 0, 0, 0, 0)
        assert par_gen.get_output() == 0

    def test_PAR_gen_16_1111110000000000(self):
        par_gen = ParityGenerator16(
            1, 1, 1, 1, 1, 1, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0)
        assert par_gen.get_output() == 0

    def test_PAR_gen_16_0000000001111111(self):
        par_gen = ParityGenerator16(
            0, 0, 0, 0, 0, 0, 0, 0, 0, 1, 1, 1, 1, 1, 1, 1)
        assert par_gen.get_output() == 1

    def test_PAR_gen_16_0000000011111110(self):
        par_gen = ParityGenerator16(
            0, 0, 0, 0, 0, 0, 0, 0, 1, 1, 1, 1, 1, 1, 1, 0)
        assert par_gen.get_output() == 1

    def test_PAR_gen_16_0000000111111100(self):
        par_gen = ParityGenerator16(
            0, 0, 0, 0, 0, 0, 0, 1, 1, 1, 1, 1, 1, 1, 0, 0)
        assert par_gen.get_output() == 1

    def test_PAR_gen_16_0000001111111000(self):
        par_gen = ParityGenerator16(
            0, 0, 0, 0, 0, 0, 1, 1, 1, 1, 1, 1, 1, 0, 0, 0)
        assert par_gen.get_output() == 1

    def test_PAR_gen_16_0000011111110000(self):
        par_gen = ParityGenerator16(
            0, 0, 0, 0, 0, 1, 1, 1, 1, 1, 1, 1, 0, 0, 0, 0)
        assert par_gen.get_output() == 1

    def test_PAR_gen_16_0000111111100000(self):
        par_gen = ParityGenerator16(
            0, 0, 0, 0, 1, 1, 1, 1, 1, 1, 1, 0, 0, 0, 0, 0)
        assert par_gen.get_output() == 1

    def test_PAR_gen_16_0001111111000000(self):
        par_gen = ParityGenerator16(
            0, 0, 0, 1, 1, 1, 1, 1, 1, 1, 0, 0, 0, 0, 0, 0)
        assert par_gen.get_output() == 1

    def test_PAR_gen_16_0011111110000000(self):
        par_gen = ParityGenerator16(
            0, 0, 1, 1, 1, 1, 1, 1, 1, 0, 0, 0, 0, 0, 0, 0)
        assert par_gen.get_output() == 1

    def test_PAR_gen_16_0111111100000000(self):
        par_gen = ParityGenerator16(
            0, 1, 1, 1, 1, 1, 1, 1, 0, 0, 0, 0, 0, 0, 0, 0)
        assert par_gen.get_output() == 1

    def test_PAR_gen_16_1111111000000000(self):
        par_gen = ParityGenerator16(
            1, 1, 1, 1, 1, 1, 1, 0, 0, 0, 0, 0, 0, 0, 0, 0)
        assert par_gen.get_output() == 1

    def test_PAR_gen_16_0000000011111111(self):
        par_gen = ParityGenerator16(
            0, 0, 0, 0, 0, 0, 0, 0, 1, 1, 1, 1, 1, 1, 1, 1)
        assert par_gen.get_output() == 0

    def test_PAR_gen_16_0000000111111110(self):
        par_gen = ParityGenerator16(
            0, 0, 0, 0, 0, 0, 0, 1, 1, 1, 1, 1, 1, 1, 1, 0)
        assert par_gen.get_output() == 0

    def test_PAR_gen_16_0000001111111100(self):
        par_gen = ParityGenerator16(
            0, 0, 0, 0, 0, 0, 1, 1, 1, 1, 1, 1, 1, 1, 0, 0)
        assert par_gen.get_output() == 0

    def test_PAR_gen_16_0000011111111000(self):
        par_gen = ParityGenerator16(
            0, 0, 0, 0, 0, 1, 1, 1, 1, 1, 1, 1, 1, 0, 0, 0)
        assert par_gen.get_output() == 0

    def test_PAR_gen_16_0000111111110000(self):
        par_gen = ParityGenerator16(
            0, 0, 0, 0, 1, 1, 1, 1, 1, 1, 1, 1, 0, 0, 0, 0)
        assert par_gen.get_output() == 0

    def test_PAR_gen_16_0001111111100000(self):
        par_gen = ParityGenerator16(
            0, 0, 0, 1, 1, 1, 1, 1, 1, 1, 1, 0, 0, 0, 0, 0)
        assert par_gen.get_output() == 0

    def test_PAR_gen_16_0011111111000000(self):
        par_gen = ParityGenerator16(
            0, 0, 1, 1, 1, 1, 1, 1, 1, 1, 0, 0, 0, 0, 0, 0)
        assert par_gen.get_output() == 0

    def test_PAR_gen_16_0111111110000000(self):
        par_gen = ParityGenerator16(
            0, 1, 1, 1, 1, 1, 1, 1, 1, 0, 0, 0, 0, 0, 0, 0)
        assert par_gen.get_output() == 0

    def test_PAR_gen_16_1111111100000000(self):
        par_gen = ParityGenerator16(
            1, 1, 1, 1, 1, 1, 1, 1, 0, 0, 0, 0, 0, 0, 0, 0)
        assert par_gen.get_output() == 0

    def test_PAR_gen_16_0000000111111111(self):
        par_gen = ParityGenerator16(
            0, 0, 0, 0, 0, 0, 0, 1, 1, 1, 1, 1, 1, 1, 1, 1)
        assert par_gen.get_output() == 1

    def test_PAR_gen_16_0000001111111110(self):
        par_gen = ParityGenerator16(
            0, 0, 0, 0, 0, 0, 1, 1, 1, 1, 1, 1, 1, 1, 1, 0)
        assert par_gen.get_output() == 1

    def test_PAR_gen_16_0000011111111100(self):
        par_gen = ParityGenerator16(
            0, 0, 0, 0, 0, 1, 1, 1, 1, 1, 1, 1, 1, 1, 0, 0)
        assert par_gen.get_output() == 1

    def test_PAR_gen_16_0000111111111000(self):
        par_gen = ParityGenerator16(
            0, 0, 0, 0, 1, 1, 1, 1, 1, 1, 1, 1, 1, 0, 0, 0)
        assert par_gen.get_output() == 1

    def test_PAR_gen_16_0001111111110000(self):
        par_gen = ParityGenerator16(
            0, 0, 0, 1, 1, 1, 1, 1, 1, 1, 1, 1, 0, 0, 0, 0)
        assert par_gen.get_output() == 1

    def test_PAR_gen_16_0011111111100000(self):
        par_gen = ParityGenerator16(
            0, 0, 1, 1, 1, 1, 1, 1, 1, 1, 1, 0, 0, 0, 0, 0)
        assert par_gen.get_output() == 1

    def test_PAR_gen_16_0111111111000000(self):
        par_gen = ParityGenerator16(
            0, 1, 1, 1, 1, 1, 1, 1, 1, 1, 0, 0, 0, 0, 0, 0)
        assert par_gen.get_output() == 1

    def test_PAR_gen_16_1111111110000000(self):
        par_gen = ParityGenerator16(
            1, 1, 1, 1, 1, 1, 1, 1, 1, 0, 0, 0, 0, 0, 0, 0)
        assert par_gen.get_output() == 1

    def test_PAR_gen_16_0000001111111111(self):
        par_gen = ParityGenerator16(
            0, 0, 0, 0, 0, 0, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1)
        assert par_gen.get_output() == 0

    def test_PAR_gen_16_0000011111111110(self):
        par_gen = ParityGenerator16(
            0, 0, 0, 0, 0, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 0)
        assert par_gen.get_output() == 0

    def test_PAR_gen_16_0000111111111100(self):
        par_gen = ParityGenerator16(
            0, 0, 0, 0, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 0, 0)
        assert par_gen.get_output() == 0

    def test_PAR_gen_16_0001111111111000(self):
        par_gen = ParityGenerator16(
            0, 0, 0, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 0, 0, 0)
        assert par_gen.get_output() == 0

    def test_PAR_gen_16_0011111111110000(self):
        par_gen = ParityGenerator16(
            0, 0, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 0, 0, 0, 0)
        assert par_gen.get_output() == 0

    def test_PAR_gen_16_0111111111100000(self):
        par_gen = ParityGenerator16(
            0, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 0, 0, 0, 0, 0)
        assert par_gen.get_output() == 0

    def test_PAR_gen_16_1111111111000000(self):
        par_gen = ParityGenerator16(
            1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 0, 0, 0, 0, 0, 0)
        assert par_gen.get_output() == 0

    def test_PAR_gen_16_0000011111111111(self):
        par_gen = ParityGenerator16(
            0, 0, 0, 0, 0, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1)
        assert par_gen.get_output() == 1

    def test_PAR_gen_16_0000111111111110(self):
        par_gen = ParityGenerator16(
            0, 0, 0, 0, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 0)
        assert par_gen.get_output() == 1

    def test_PAR_gen_16_0001111111111100(self):
        par_gen = ParityGenerator16(
            0, 0, 0, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 0, 0)
        assert par_gen.get_output() == 1

    def test_PAR_gen_16_0011111111111000(self):
        par_gen = ParityGenerator16(
            0, 0, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 0, 0, 0)
        assert par_gen.get_output() == 1

    def test_PAR_gen_16_0111111111110000(self):
        par_gen = ParityGenerator16(
            0, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 0, 0, 0, 0)
        assert par_gen.get_output() == 1

    def test_PAR_gen_16_1111111111100000(self):
        par_gen = ParityGenerator16(
            1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 0, 0, 0, 0, 0)
        assert par_gen.get_output() == 1

    def test_PAR_gen_16_0000111111111111(self):
        par_gen = ParityGenerator16(
            0, 0, 0, 0, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1)
        assert par_gen.get_output() == 0

    def test_PAR_gen_16_0001111111111110(self):
        par_gen = ParityGenerator16(
            0, 0, 0, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 0)
        assert par_gen.get_output() == 0

    def test_PAR_gen_16_0011111111111100(self):
        par_gen = ParityGenerator16(
            0, 0, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 0, 0)
        assert par_gen.get_output() == 0

    def test_PAR_gen_16_0111111111111000(self):
        par_gen = ParityGenerator16(
            0, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 0, 0, 0)
        assert par_gen.get_output() == 0

    def test_PAR_gen_16_1111111111110000(self):
        par_gen = ParityGenerator16(
            1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 0, 0, 0, 0)
        assert par_gen.get_output() == 0

    def test_PAR_gen_16_0001111111111111(self):
        par_gen = ParityGenerator16(
            0, 0, 0, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1)
        assert par_gen.get_output() == 1

    def test_PAR_gen_16_0011111111111110(self):
        par_gen = ParityGenerator16(
            0, 0, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 0)
        assert par_gen.get_output() == 1

    def test_PAR_gen_16_0111111111111100(self):
        par_gen = ParityGenerator16(
            0, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 0, 0)
        assert par_gen.get_output() == 1

    def test_PAR_gen_16_1111111111111000(self):
        par_gen = ParityGenerator16(
            1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 0, 0, 0)
        assert par_gen.get_output() == 1

    def test_PAR_gen_16_0011111111111111(self):
        par_gen = ParityGenerator16(
            0, 0, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1)
        assert par_gen.get_output() == 0

    def test_PAR_gen_16_0111111111111110(self):
        par_gen = ParityGenerator16(
            0, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 0)
        assert par_gen.get_output() == 0

    def test_PAR_gen_16_1111111111111100(self):
        par_gen = ParityGenerator16(
            1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 0, 0)
        assert par_gen.get_output() == 0

    def test_PAR_gen_16_0111111111111111(self):
        par_gen = ParityGenerator16(
            0, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1)
        assert par_gen.get_output() == 1

    def test_PAR_gen_16_1111111111111110(self):
        par_gen = ParityGenerator16(
            1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 0)
        assert par_gen.get_output() == 1

    def test_PAR_gen_16_1111111111111111(self):
        par_gen = ParityGenerator16(
            1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1)
        assert par_gen.get_output() == 0
