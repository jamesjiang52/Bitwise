import bitwise as bw


class TestIMPLYGate:
    def test_IMPLYGate(self):
        input_1 = bw.wire.Wire()
        input_2 = bw.wire.Wire()
        output = bw.wire.Wire()

        a = bw.gate.IMPLYGate(input_1, input_2, output)

        input_1.value = 0
        input_2.value = 0
        assert output.value == 1

        input_1.value = 0
        input_2.value = 1
        assert output.value == 1

        input_1.value = 1
        input_2.value = 0
        assert output.value == 0

        input_1.value = 1
        input_2.value = 1
        assert output.value == 1

        print(a.__doc__)
        print(a)

        a(input_1=0, input_2=0, output=None)
        assert output.value == 1
