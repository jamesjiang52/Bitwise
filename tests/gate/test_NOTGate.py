import bitwise as bw


class TestNOTGate:
    def test_NOTGate(self):
        input_1 = bw.wire.Wire()
        output = bw.wire.Wire()

        a = bw.gate.NOTGate(input_1, output)

        input_1.value = 0
        assert output.value == 1

        input_1.value = 1
        assert output.value == 0

        print(a.__doc__)
        print(a)

        a(input=0, output=None)
        assert output.value == 1
