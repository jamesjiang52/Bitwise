import sys
sys.path.insert(0, "../../")
import logic
from PAR import PAROf4Gen


class TestPAROf4Gen:
    def test_PAR_of4_gen_0000(self):
        par_gen = PAROf4Gen(0, 0, 0, 0)
        assert par_gen.get_output() == 0

    def test_PAR_of4_gen_0001(self):
        par_gen = PAROf4Gen(0, 0, 0, 1)
        assert par_gen.get_output() == 1

    def test_PAR_of4_gen_0010(self):
        par_gen = PAROf4Gen(0, 0, 1, 0)
        assert par_gen.get_output() == 1

    def test_PAR_of4_gen_0100(self):
        par_gen = PAROf4Gen(0, 1, 0, 0)
        assert par_gen.get_output() == 1

    def test_PAR_of4_gen_1000(self):
        par_gen = PAROf4Gen(1, 0, 0, 0)
        assert par_gen.get_output() == 1

    def test_PAR_of4_gen_0011(self):
        par_gen = PAROf4Gen(0, 0, 1, 1)
        assert par_gen.get_output() == 0

    def test_PAR_of4_gen_0110(self):
        par_gen = PAROf4Gen(0, 1, 1, 0)
        assert par_gen.get_output() == 0

    def test_PAR_of4_gen_1100(self):
        par_gen = PAROf4Gen(1, 1, 0, 0)
        assert par_gen.get_output() == 0

    def test_PAR_of4_gen_0111(self):
        par_gen = PAROf4Gen(0, 1, 1, 1)
        assert par_gen.get_output() == 1

    def test_PAR_of4_gen_1110(self):
        par_gen = PAROf4Gen(1, 1, 1, 0)
        assert par_gen.get_output() == 1

    def test_PAR_of4_gen_1111(self):
        par_gen = PAROf4Gen(1, 1, 1, 1)
        assert par_gen.get_output() == 0
