import bitwise as bw


class TestEncoder8To3:
    def test_Encoder8To3(self):
        enable = bw.wire.Wire()
        input_1 = bw.wire.Wire()
        input_2 = bw.wire.Wire()
        input_3 = bw.wire.Wire()
        input_4 = bw.wire.Wire()
        input_5 = bw.wire.Wire()
        input_6 = bw.wire.Wire()
        input_7 = bw.wire.Wire()
        input_8 = bw.wire.Wire()
        valid = bw.wire.Wire()
        output_1 = bw.wire.Wire()
        output_2 = bw.wire.Wire()
        output_3 = bw.wire.Wire()
        input_bus = bw.wire.Bus8(
            input_1,
            input_2,
            input_3,
            input_4,
            input_5,
            input_6,
            input_7,
            input_8
        )

        a = bw.signal.Encoder8To3(
            enable,
            input_bus,
            valid,
            output_1,
            output_2,
            output_3
        )

        enable.value = 0
        input_1.value = 0
        input_2.value = 0
        input_3.value = 0
        input_4.value = 0
        input_5.value = 0
        input_6.value = 0
        input_7.value = 0
        input_8.value = 0
        assert valid.value == 0
        assert output_1.value == 0
        assert output_2.value == 0
        assert output_3.value == 0

        enable.value = 0
        input_1.value = 1
        input_2.value = 1
        input_3.value = 1
        input_4.value = 1
        input_5.value = 1
        input_6.value = 1
        input_7.value = 1
        input_8.value = 1
        assert valid.value == 0
        assert output_1.value == 0
        assert output_2.value == 0
        assert output_3.value == 0

        enable.value = 1
        input_1.value = 0
        input_2.value = 0
        input_3.value = 0
        input_4.value = 0
        input_5.value = 0
        input_6.value = 0
        input_7.value = 0
        input_8.value = 0
        assert valid.value == 0
        assert output_1.value == 0
        assert output_2.value == 0
        assert output_3.value == 0

        enable.value = 1
        input_1.value = 0
        input_2.value = 0
        input_3.value = 0
        input_4.value = 0
        input_5.value = 0
        input_6.value = 0
        input_7.value = 0
        input_8.value = 1
        assert valid.value == 1
        assert output_1.value == 0
        assert output_2.value == 0
        assert output_3.value == 0

        enable.value = 1
        input_1.value = 0
        input_2.value = 0
        input_3.value = 0
        input_4.value = 0
        input_5.value = 0
        input_6.value = 0
        input_7.value = 1
        input_8.value = 0
        assert valid.value == 1
        assert output_1.value == 0
        assert output_2.value == 0
        assert output_3.value == 1

        enable.value = 1
        input_1.value = 0
        input_2.value = 0
        input_3.value = 0
        input_4.value = 0
        input_5.value = 0
        input_6.value = 0
        input_7.value = 1
        input_8.value = 1
        assert valid.value == 1
        assert output_1.value == 0
        assert output_2.value == 0
        assert output_3.value == 1

        enable.value = 1
        input_1.value = 0
        input_2.value = 0
        input_3.value = 0
        input_4.value = 0
        input_5.value = 0
        input_6.value = 1
        input_7.value = 0
        input_8.value = 0
        assert valid.value == 1
        assert output_1.value == 0
        assert output_2.value == 1
        assert output_3.value == 0

        enable.value = 1
        input_1.value = 0
        input_2.value = 0
        input_3.value = 0
        input_4.value = 0
        input_5.value = 0
        input_6.value = 1
        input_7.value = 1
        input_8.value = 1
        assert valid.value == 1
        assert output_1.value == 0
        assert output_2.value == 1
        assert output_3.value == 0

        enable.value = 1
        input_1.value = 0
        input_2.value = 0
        input_3.value = 0
        input_4.value = 0
        input_5.value = 1
        input_6.value = 0
        input_7.value = 0
        input_8.value = 0
        assert valid.value == 1
        assert output_1.value == 0
        assert output_2.value == 1
        assert output_3.value == 1

        enable.value = 1
        input_1.value = 0
        input_2.value = 0
        input_3.value = 0
        input_4.value = 0
        input_5.value = 1
        input_6.value = 1
        input_7.value = 1
        input_8.value = 1
        assert valid.value == 1
        assert output_1.value == 0
        assert output_2.value == 1
        assert output_3.value == 1

        enable.value = 1
        input_1.value = 0
        input_2.value = 0
        input_3.value = 0
        input_4.value = 1
        input_5.value = 0
        input_6.value = 0
        input_7.value = 0
        input_8.value = 0
        assert valid.value == 1
        assert output_1.value == 1
        assert output_2.value == 0
        assert output_3.value == 0

        enable.value = 1
        input_1.value = 0
        input_2.value = 0
        input_3.value = 0
        input_4.value = 1
        input_5.value = 1
        input_6.value = 1
        input_7.value = 1
        input_8.value = 1
        assert valid.value == 1
        assert output_1.value == 1
        assert output_2.value == 0
        assert output_3.value == 0

        enable.value = 1
        input_1.value = 0
        input_2.value = 0
        input_3.value = 1
        input_4.value = 0
        input_5.value = 0
        input_6.value = 0
        input_7.value = 0
        input_8.value = 0
        assert valid.value == 1
        assert output_1.value == 1
        assert output_2.value == 0
        assert output_3.value == 1

        enable.value = 1
        input_1.value = 0
        input_2.value = 0
        input_3.value = 1
        input_4.value = 1
        input_5.value = 1
        input_6.value = 1
        input_7.value = 1
        input_8.value = 1
        assert valid.value == 1
        assert output_1.value == 1
        assert output_2.value == 0
        assert output_3.value == 1

        enable.value = 1
        input_1.value = 0
        input_2.value = 1
        input_3.value = 0
        input_4.value = 0
        input_5.value = 0
        input_6.value = 0
        input_7.value = 0
        input_8.value = 0
        assert valid.value == 1
        assert output_1.value == 1
        assert output_2.value == 1
        assert output_3.value == 0

        enable.value = 1
        input_1.value = 0
        input_2.value = 1
        input_3.value = 1
        input_4.value = 1
        input_5.value = 1
        input_6.value = 1
        input_7.value = 1
        input_8.value = 1
        assert valid.value == 1
        assert output_1.value == 1
        assert output_2.value == 1
        assert output_3.value == 0

        enable.value = 1
        input_1.value = 1
        input_2.value = 0
        input_3.value = 0
        input_4.value = 0
        input_5.value = 0
        input_6.value = 0
        input_7.value = 0
        input_8.value = 0
        assert valid.value == 1
        assert output_1.value == 1
        assert output_2.value == 1
        assert output_3.value == 1

        enable.value = 1
        input_1.value = 1
        input_2.value = 1
        input_3.value = 1
        input_4.value = 1
        input_5.value = 1
        input_6.value = 1
        input_7.value = 1
        input_8.value = 1
        assert valid.value == 1
        assert output_1.value == 1
        assert output_2.value == 1
        assert output_3.value == 1

        print(a.__doc__)
        print(a)

        a(
            enable=1,
            input_bus=(0, 0, 0, 0, 0, 0, 0, 1),
            valid=None,
            output_1=None,
            output_2=None,
            output_3=None
        )
        assert valid.value == 1
        assert output_1.value == 0
        assert output_2.value == 0
        assert output_3.value == 0
