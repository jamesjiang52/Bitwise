import sys
sys.path.insert(0, "../../")
import logic
from MAJ import MAJOf4


class TestMAJOf4:
    def test_MAJ_of4_0000(self):
        maj = MAJOf4(0, 0, 0, 0)
        assert maj.get_output() == 0

    def test_MAJ_of4_0001(self):
        maj = MAJOf4(0, 0, 0, 1)
        assert maj.get_output() == 0

    def test_MAJ_of4_0010(self):
        maj = MAJOf4(0, 0, 1, 0)
        assert maj.get_output() == 0

    def test_MAJ_of4_0011(self):
        maj = MAJOf4(0, 0, 1, 1)
        assert maj.get_output() == 0

    def test_MAJ_of4_0100(self):
        maj = MAJOf4(0, 1, 0, 0)
        assert maj.get_output() == 0

    def test_MAJ_of4_0101(self):
        maj = MAJOf4(0, 1, 0, 1)
        assert maj.get_output() == 0

    def test_MAJ_of4_0110(self):
        maj = MAJOf4(0, 1, 1, 0)
        assert maj.get_output() == 0

    def test_MAJ_of4_0111(self):
        maj = MAJOf4(0, 1, 1, 1)
        assert maj.get_output() == 1

    def test_MAJ_of4_1000(self):
        maj = MAJOf4(1, 0, 0, 0)
        assert maj.get_output() == 0

    def test_MAJ_of4_1001(self):
        maj = MAJOf4(1, 0, 0, 1)
        assert maj.get_output() == 0

    def test_MAJ_of4_1010(self):
        maj = MAJOf4(1, 0, 1, 0)
        assert maj.get_output() == 0

    def test_MAJ_of4_1011(self):
        maj = MAJOf4(1, 0, 1, 1)
        assert maj.get_output() == 1

    def test_MAJ_of4_1100(self):
        maj = MAJOf4(1, 1, 0, 0)
        assert maj.get_output() == 0

    def test_MAJ_of4_1101(self):
        maj = MAJOf4(1, 1, 0, 1)
        assert maj.get_output() == 1

    def test_MAJ_of4_1110(self):
        maj = MAJOf4(1, 1, 1, 0)
        assert maj.get_output() == 1

    def test_MAJ_of4_1111(self):
        maj = MAJOf4(1, 1, 1, 1)
        assert maj.get_output() == 1
