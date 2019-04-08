import bitwise as bw


class TestANDGate2:
    def test_ANDGate2(self):
        input_1 = bw.wire.Wire()
        input_2 = bw.wire.Wire()
        output = bw.wire.Wire()

        a = bw.gate.ANDGate2(input_1, input_2, output)

        input_1.value = 0
        input_2.value = 0
        assert output.value == 0

        input_1.value = 0
        input_2.value = 1
        assert output.value == 0

        input_1.value = 1
        input_2.value = 0
        assert output.value == 0

        input_1.value = 1
        input_2.value = 1
        assert output.value == 1

        print(a.__doc__)
        print(a)
