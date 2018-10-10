import bitwise as bw


class TestMultiplexer8To1:
    def test_Multiplexer8To1(self):
        enable = bw.wire.Wire()
        select_1 = bw.wire.Wire()
        select_2 = bw.wire.Wire()
        select_3 = bw.wire.Wire()
        input_1 = bw.wire.Wire()
        input_2 = bw.wire.Wire()
        input_3 = bw.wire.Wire()
        input_4 = bw.wire.Wire()
        input_5 = bw.wire.Wire()
        input_6 = bw.wire.Wire()
        input_7 = bw.wire.Wire()
        input_8 = bw.wire.Wire()
        output = bw.wire.Wire()
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

        bw.signal.Multiplexer8To1(
            enable,
            select_1,
            select_2,
            select_3,
            input_bus,
            output
        )

        enable.value = 0
        select_1.value = 0
        select_2.value = 0
        select_3.value = 0
        input_1.value = 0
        input_2.value = 0
        input_3.value = 0
        input_4.value = 0
        input_5.value = 0
        input_6.value = 0
        input_7.value = 0
        input_8.value = 0
        assert output.value == 0

        enable.value = 0
        select_1.value = 1
        select_2.value = 1
        select_3.value = 1
        input_1.value = 1
        input_2.value = 1
        input_3.value = 1
        input_4.value = 1
        input_5.value = 1
        input_6.value = 1
        input_7.value = 1
        input_8.value = 1
        assert output.value == 0

        enable.value = 1
        select_1.value = 0
        select_2.value = 0
        select_3.value = 0
        input_1.value = 0
        input_2.value = 0
        input_3.value = 0
        input_4.value = 0
        input_5.value = 0
        input_6.value = 0
        input_7.value = 0
        input_8.value = 0
        assert output.value == 0

        enable.value = 1
        select_1.value = 0
        select_2.value = 0
        select_3.value = 0
        input_1.value = 0
        input_2.value = 0
        input_3.value = 0
        input_4.value = 0
        input_5.value = 0
        input_6.value = 0
        input_7.value = 0
        input_8.value = 1
        assert output.value == 1

        enable.value = 1
        select_1.value = 0
        select_2.value = 0
        select_3.value = 0
        input_1.value = 1
        input_2.value = 1
        input_3.value = 1
        input_4.value = 1
        input_5.value = 1
        input_6.value = 1
        input_7.value = 1
        input_8.value = 0
        assert output.value == 0

        enable.value = 1
        select_1.value = 0
        select_2.value = 0
        select_3.value = 0
        input_1.value = 1
        input_2.value = 1
        input_3.value = 1
        input_4.value = 1
        input_5.value = 1
        input_6.value = 1
        input_7.value = 1
        input_8.value = 1
        assert output.value == 1

        enable.value = 1
        select_1.value = 0
        select_2.value = 0
        select_3.value = 1
        input_1.value = 0
        input_2.value = 0
        input_3.value = 0
        input_4.value = 0
        input_5.value = 0
        input_6.value = 0
        input_7.value = 0
        input_8.value = 0
        assert output.value == 0

        enable.value = 1
        select_1.value = 0
        select_2.value = 0
        select_3.value = 1
        input_1.value = 0
        input_2.value = 0
        input_3.value = 0
        input_4.value = 0
        input_5.value = 0
        input_6.value = 0
        input_7.value = 1
        input_8.value = 0
        assert output.value == 1

        enable.value = 1
        select_1.value = 0
        select_2.value = 0
        select_3.value = 1
        input_1.value = 1
        input_2.value = 1
        input_3.value = 1
        input_4.value = 1
        input_5.value = 1
        input_6.value = 1
        input_7.value = 0
        input_8.value = 1
        assert output.value == 0

        enable.value = 1
        select_1.value = 0
        select_2.value = 0
        select_3.value = 1
        input_1.value = 1
        input_2.value = 1
        input_3.value = 1
        input_4.value = 1
        input_5.value = 1
        input_6.value = 1
        input_7.value = 1
        input_8.value = 1
        assert output.value == 1

        enable.value = 1
        select_1.value = 0
        select_2.value = 1
        select_3.value = 0
        input_1.value = 0
        input_2.value = 0
        input_3.value = 0
        input_4.value = 0
        input_5.value = 0
        input_6.value = 0
        input_7.value = 0
        input_8.value = 0
        assert output.value == 0

        enable.value = 1
        select_1.value = 0
        select_2.value = 1
        select_3.value = 0
        input_1.value = 0
        input_2.value = 0
        input_3.value = 0
        input_4.value = 0
        input_5.value = 0
        input_6.value = 1
        input_7.value = 0
        input_8.value = 0
        assert output.value == 1

        enable.value = 1
        select_1.value = 0
        select_2.value = 1
        select_3.value = 0
        input_1.value = 1
        input_2.value = 1
        input_3.value = 1
        input_4.value = 1
        input_5.value = 1
        input_6.value = 0
        input_7.value = 1
        input_8.value = 1
        assert output.value == 0

        enable.value = 1
        select_1.value = 0
        select_2.value = 1
        select_3.value = 0
        input_1.value = 1
        input_2.value = 1
        input_3.value = 1
        input_4.value = 1
        input_5.value = 1
        input_6.value = 1
        input_7.value = 1
        input_8.value = 1
        assert output.value == 1

        enable.value = 1
        select_1.value = 0
        select_2.value = 1
        select_3.value = 1
        input_1.value = 0
        input_2.value = 0
        input_3.value = 0
        input_4.value = 0
        input_5.value = 0
        input_6.value = 0
        input_7.value = 0
        input_8.value = 0
        assert output.value == 0

        enable.value = 1
        select_1.value = 0
        select_2.value = 1
        select_3.value = 1
        input_1.value = 0
        input_2.value = 0
        input_3.value = 0
        input_4.value = 0
        input_5.value = 1
        input_6.value = 0
        input_7.value = 0
        input_8.value = 0
        assert output.value == 1

        enable.value = 1
        select_1.value = 0
        select_2.value = 1
        select_3.value = 1
        input_1.value = 1
        input_2.value = 1
        input_3.value = 1
        input_4.value = 1
        input_5.value = 0
        input_6.value = 1
        input_7.value = 1
        input_8.value = 1
        assert output.value == 0

        enable.value = 1
        select_1.value = 0
        select_2.value = 1
        select_3.value = 1
        input_1.value = 1
        input_2.value = 1
        input_3.value = 1
        input_4.value = 1
        input_5.value = 1
        input_6.value = 1
        input_7.value = 1
        input_8.value = 1
        assert output.value == 1

        enable.value = 1
        select_1.value = 1
        select_2.value = 0
        select_3.value = 0
        input_1.value = 0
        input_2.value = 0
        input_3.value = 0
        input_4.value = 0
        input_5.value = 0
        input_6.value = 0
        input_7.value = 0
        input_8.value = 0
        assert output.value == 0

        enable.value = 1
        select_1.value = 1
        select_2.value = 0
        select_3.value = 0
        input_1.value = 0
        input_2.value = 0
        input_3.value = 0
        input_4.value = 1
        input_5.value = 0
        input_6.value = 0
        input_7.value = 0
        input_8.value = 0
        assert output.value == 1

        enable.value = 1
        select_1.value = 1
        select_2.value = 0
        select_3.value = 0
        input_1.value = 1
        input_2.value = 1
        input_3.value = 1
        input_4.value = 0
        input_5.value = 1
        input_6.value = 1
        input_7.value = 1
        input_8.value = 1
        assert output.value == 0

        enable.value = 1
        select_1.value = 1
        select_2.value = 0
        select_3.value = 0
        input_1.value = 1
        input_2.value = 1
        input_3.value = 1
        input_4.value = 1
        input_5.value = 1
        input_6.value = 1
        input_7.value = 1
        input_8.value = 1
        assert output.value == 1

        enable.value = 1
        select_1.value = 1
        select_2.value = 0
        select_3.value = 1
        input_1.value = 0
        input_2.value = 0
        input_3.value = 0
        input_4.value = 0
        input_5.value = 0
        input_6.value = 0
        input_7.value = 0
        input_8.value = 0
        assert output.value == 0

        enable.value = 1
        select_1.value = 1
        select_2.value = 0
        select_3.value = 1
        input_1.value = 0
        input_2.value = 0
        input_3.value = 1
        input_4.value = 0
        input_5.value = 0
        input_6.value = 0
        input_7.value = 0
        input_8.value = 0
        assert output.value == 1

        enable.value = 1
        select_1.value = 1
        select_2.value = 0
        select_3.value = 1
        input_1.value = 1
        input_2.value = 1
        input_3.value = 0
        input_4.value = 1
        input_5.value = 1
        input_6.value = 1
        input_7.value = 1
        input_8.value = 1
        assert output.value == 0

        enable.value = 1
        select_1.value = 1
        select_2.value = 0
        select_3.value = 1
        input_1.value = 1
        input_2.value = 1
        input_3.value = 1
        input_4.value = 1
        input_5.value = 1
        input_6.value = 1
        input_7.value = 1
        input_8.value = 1
        assert output.value == 1

        enable.value = 1
        select_1.value = 1
        select_2.value = 1
        select_3.value = 0
        input_1.value = 0
        input_2.value = 0
        input_3.value = 0
        input_4.value = 0
        input_5.value = 0
        input_6.value = 0
        input_7.value = 0
        input_8.value = 0
        assert output.value == 0

        enable.value = 1
        select_1.value = 1
        select_2.value = 1
        select_3.value = 0
        input_1.value = 0
        input_2.value = 1
        input_3.value = 0
        input_4.value = 0
        input_5.value = 0
        input_6.value = 0
        input_7.value = 0
        input_8.value = 0
        assert output.value == 1

        enable.value = 1
        select_1.value = 1
        select_2.value = 1
        select_3.value = 0
        input_1.value = 1
        input_2.value = 0
        input_3.value = 1
        input_4.value = 1
        input_5.value = 1
        input_6.value = 1
        input_7.value = 1
        input_8.value = 1
        assert output.value == 0

        enable.value = 1
        select_1.value = 1
        select_2.value = 1
        select_3.value = 0
        input_1.value = 1
        input_2.value = 1
        input_3.value = 1
        input_4.value = 1
        input_5.value = 1
        input_6.value = 1
        input_7.value = 1
        input_8.value = 1
        assert output.value == 1

        enable.value = 1
        select_1.value = 1
        select_2.value = 1
        select_3.value = 1
        input_1.value = 0
        input_2.value = 0
        input_3.value = 0
        input_4.value = 0
        input_5.value = 0
        input_6.value = 0
        input_7.value = 0
        input_8.value = 0
        assert output.value == 0

        enable.value = 1
        select_1.value = 1
        select_2.value = 1
        select_3.value = 1
        input_1.value = 1
        input_2.value = 0
        input_3.value = 0
        input_4.value = 0
        input_5.value = 0
        input_6.value = 0
        input_7.value = 0
        input_8.value = 0
        assert output.value == 1

        enable.value = 1
        select_1.value = 1
        select_2.value = 1
        select_3.value = 1
        input_1.value = 0
        input_2.value = 1
        input_3.value = 1
        input_4.value = 1
        input_5.value = 1
        input_6.value = 1
        input_7.value = 1
        input_8.value = 1
        assert output.value == 0

        enable.value = 1
        select_1.value = 1
        select_2.value = 1
        select_3.value = 1
        input_1.value = 1
        input_2.value = 1
        input_3.value = 1
        input_4.value = 1
        input_5.value = 1
        input_6.value = 1
        input_7.value = 1
        input_8.value = 1
        assert output.value == 1
