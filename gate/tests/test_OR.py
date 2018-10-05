import sys
sys.path.insert(0, "../../")
from wire.WIRE import Wire
from gate import OR


class TestOR:
    def test_ORGate2(self):
        input_1 = Wire(0)
        input_2 = Wire(0)
        output = Wire(0)

        OR.ORGate2(input_1, input_2, output)

        input_1.value = 0
        input_2.value = 0
        assert output.value ==  0

        input_1.value = 0
        input_2.value = 1
        assert output.value ==  1

        input_1.value = 1
        input_2.value = 0
        assert output.value ==  1

        input_1.value = 1
        input_2.value = 1
        assert output.value ==  1

    def test_ORGate3(self):
        input_1 = Wire(0)
        input_2 = Wire(0)
        input_3 = Wire(0)
        output = Wire(0)

        OR.ORGate3(input_1, input_2, input_3, output)

        input_1.value = 0
        input_2.value = 0
        input_3.value = 0
        assert output.value == 0

        input_1.value = 0
        input_2.value = 0
        input_3.value = 1
        assert output.value == 1

        input_1.value = 0
        input_2.value = 1
        input_3.value = 0
        assert output.value == 1

        input_1.value = 0
        input_2.value = 1
        input_3.value = 1
        assert output.value == 1

        input_1.value = 1
        input_2.value = 0
        input_3.value = 0
        assert output.value == 1

        input_1.value = 1
        input_2.value = 0
        input_3.value = 1
        assert output.value == 1

        input_1.value = 1
        input_2.value = 1
        input_3.value = 0
        assert output.value == 1

        input_1.value = 1
        input_2.value = 1
        input_3.value = 1
        assert output.value == 1

    def test_ORGate4(self):
        input_1 = Wire(0)
        input_2 = Wire(0)
        input_3 = Wire(0)
        input_4 = Wire(0)
        output = Wire(0)

        OR.ORGate4(input_1, input_2, input_3, input_4, output)

        input_1.value = 0
        input_2.value = 0
        input_3.value = 0
        input_4.value = 0
        assert output.value == 0

        input_1.value = 0
        input_2.value = 0
        input_3.value = 0
        input_4.value = 1
        assert output.value == 1

        input_1.value = 0
        input_2.value = 0
        input_3.value = 1
        input_4.value = 0
        assert output.value == 1

        input_1.value = 0
        input_2.value = 0
        input_3.value = 1
        input_4.value = 1
        assert output.value == 1

        input_1.value = 0
        input_2.value = 1
        input_3.value = 0
        input_4.value = 0
        assert output.value == 1

        input_1.value = 0
        input_2.value = 1
        input_3.value = 0
        input_4.value = 1
        assert output.value == 1

        input_1.value = 0
        input_2.value = 1
        input_3.value = 1
        input_4.value = 0
        assert output.value == 1

        input_1.value = 0
        input_2.value = 1
        input_3.value = 1
        input_4.value = 1
        assert output.value == 1

        input_1.value = 1
        input_2.value = 0
        input_3.value = 0
        input_4.value = 0
        assert output.value == 1

        input_1.value = 1
        input_2.value = 0
        input_3.value = 0
        input_4.value = 1
        assert output.value == 1

        input_1.value = 1
        input_2.value = 0
        input_3.value = 1
        input_4.value = 0
        assert output.value == 1

        input_1.value = 1
        input_2.value = 0
        input_3.value = 1
        input_4.value = 1
        assert output.value == 1

        input_1.value = 1
        input_2.value = 1
        input_3.value = 0
        input_4.value = 0
        assert output.value == 1

        input_1.value = 1
        input_2.value = 1
        input_3.value = 0
        input_4.value = 1
        assert output.value == 1

        input_1.value = 1
        input_2.value = 1
        input_3.value = 1
        input_4.value = 0
        assert output.value == 1

        input_1.value = 1
        input_2.value = 1
        input_3.value = 1
        input_4.value = 1
        assert output.value == 1
