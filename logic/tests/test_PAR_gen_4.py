import sys
sys.path.insert(0, "../../")
import logic
from PAR import ParityGenerator4


class TestParityGenerator4:
    def test_PAR_gen_4_0000(self):
        par_gen = ParityGenerator4(0, 0, 0, 0)
        assert par_gen.get_output() == 0

    def test_PAR_gen_4_0001(self):
        par_gen = ParityGenerator4(0, 0, 0, 1)
        assert par_gen.get_output() == 1

    def test_PAR_gen_4_0010(self):
        par_gen = ParityGenerator4(0, 0, 1, 0)
        assert par_gen.get_output() == 1

    def test_PAR_gen_4_0100(self):
        par_gen = ParityGenerator4(0, 1, 0, 0)
        assert par_gen.get_output() == 1

    def test_PAR_gen_4_1000(self):
        par_gen = ParityGenerator4(1, 0, 0, 0)
        assert par_gen.get_output() == 1

    def test_PAR_gen_4_0011(self):
        par_gen = ParityGenerator4(0, 0, 1, 1)
        assert par_gen.get_output() == 0

    def test_PAR_gen_4_0110(self):
        par_gen = ParityGenerator4(0, 1, 1, 0)
        assert par_gen.get_output() == 0

    def test_PAR_gen_4_1100(self):
        par_gen = ParityGenerator4(1, 1, 0, 0)
        assert par_gen.get_output() == 0

    def test_PAR_gen_4_0111(self):
        par_gen = ParityGenerator4(0, 1, 1, 1)
        assert par_gen.get_output() == 1

    def test_PAR_gen_4_1110(self):
        par_gen = ParityGenerator4(1, 1, 1, 0)
        assert par_gen.get_output() == 1

    def test_PAR_gen_4_1111(self):
        par_gen = ParityGenerator4(1, 1, 1, 1)
        assert par_gen.get_output() == 0
