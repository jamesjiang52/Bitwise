import sys
sys.path.insert(0, "../../")
from wire.WIRE import Wire
from gate import NOT


class TestNOT:
    def test_NOT(self):
        input_1 = Wire(0)
        output = Wire(0)

        NOT.NOTGate(input_1, output)

        input_1.value = 0
        assert output.value == 1

        input_1.value = 1
        assert output.value == 0
