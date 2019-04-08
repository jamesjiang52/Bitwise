import bitwise as bw


class TestORGate3:
    def test_ORGate3(self):
        input_1 = bw.wire.Wire()
        input_2 = bw.wire.Wire()
        input_3 = bw.wire.Wire()
        output = bw.wire.Wire()

        a = bw.gate.ORGate3(input_1, input_2, input_3, output)

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

        print(a.__doc__)
        print(a)
