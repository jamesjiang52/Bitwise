import bitwise as bw


class TestORGate2:
    def test_ORGate2(self):
        input_1 = bw.wire.Wire()
        input_2 = bw.wire.Wire()
        output = bw.wire.Wire()

        bw.gate.ORGate2(input_1, input_2, output)

        input_1.value = 0
        input_2.value = 0
        assert output.value == 0

        input_1.value = 0
        input_2.value = 1
        assert output.value == 1

        input_1.value = 1
        input_2.value = 0
        assert output.value == 1

        input_1.value = 1
        input_2.value = 1
        assert output.value == 1
