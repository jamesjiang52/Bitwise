import bitwise as bw


class TestNANDGate4:
    def test_NANDGate4(self):
        input_1 = bw.wire.Wire()
        input_2 = bw.wire.Wire()
        input_3 = bw.wire.Wire()
        input_4 = bw.wire.Wire()
        output = bw.wire.Wire()

        a = bw.gate.NANDGate4(input_1, input_2, input_3, input_4, output)

        input_1.value = 0
        input_2.value = 0
        input_3.value = 0
        input_4.value = 0
        assert output.value == 1

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
        assert output.value == 0

        print(a.__doc__)
        print(a)

        a(input_1=0, input_2=0, input_3=0, input_4=0, output=None)
        assert output.value == 1
