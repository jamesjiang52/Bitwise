import bitwise as bw


class TestBuffer:
    def test_Buffer(self):
        input_1 = bw.wire.Wire()
        output = bw.wire.Wire()

        a = bw.gate.Buffer(input_1, output)

        input_1.value = 0
        assert output.value == 0

        input_1.value = 1
        assert output.value == 1

        print(a.__doc__)
        print(a)

        a(input=0, output=None)
        assert output.value == 0
