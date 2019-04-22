import bitwise as bw


class TestEncoder4To2:
    def test_Encoder4To2(self):
        enable = bw.wire.Wire()
        input_1 = bw.wire.Wire()
        input_2 = bw.wire.Wire()
        input_3 = bw.wire.Wire()
        input_4 = bw.wire.Wire()
        valid = bw.wire.Wire()
        output_1 = bw.wire.Wire()
        output_2 = bw.wire.Wire()
        input_bus = bw.wire.Bus4(input_1, input_2, input_3, input_4)

        a = bw.signal.Encoder4To2(enable, input_bus, valid, output_1, output_2)

        enable.value = 0
        input_1.value = 0
        input_2.value = 0
        input_3.value = 0
        input_4.value = 0
        assert valid.value == 0
        assert output_1.value == 0
        assert output_2.value == 0

        enable.value = 0
        input_1.value = 1
        input_2.value = 1
        input_3.value = 1
        input_4.value = 1
        assert valid.value == 0
        assert output_1.value == 0
        assert output_2.value == 0

        enable.value = 1
        input_1.value = 0
        input_2.value = 0
        input_3.value = 0
        input_4.value = 0
        assert valid.value == 0
        assert output_1.value == 0
        assert output_2.value == 0

        enable.value = 1
        input_1.value = 0
        input_2.value = 0
        input_3.value = 0
        input_4.value = 1
        assert valid.value == 1
        assert output_1.value == 0
        assert output_2.value == 0

        enable.value = 1
        input_1.value = 0
        input_2.value = 0
        input_3.value = 1
        input_4.value = 0
        assert valid.value == 1
        assert output_1.value == 0
        assert output_2.value == 1

        enable.value = 1
        input_1.value = 0
        input_2.value = 0
        input_3.value = 1
        input_4.value = 1
        assert valid.value == 1
        assert output_1.value == 0
        assert output_2.value == 1

        enable.value = 1
        input_1.value = 0
        input_2.value = 1
        input_3.value = 0
        input_4.value = 0
        assert valid.value == 1
        assert output_1.value == 1
        assert output_2.value == 0

        enable.value = 1
        input_1.value = 0
        input_2.value = 1
        input_3.value = 1
        input_4.value = 1
        assert valid.value == 1
        assert output_1.value == 1
        assert output_2.value == 0

        enable.value = 1
        input_1.value = 1
        input_2.value = 0
        input_3.value = 0
        input_4.value = 0
        assert valid.value == 1
        assert output_1.value == 1
        assert output_2.value == 1

        enable.value = 1
        input_1.value = 1
        input_2.value = 1
        input_3.value = 1
        input_4.value = 1
        assert valid.value == 1
        assert output_1.value == 1
        assert output_2.value == 1

        print(a.__doc__)
        print(a)
