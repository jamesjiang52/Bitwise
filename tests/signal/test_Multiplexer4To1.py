import bitwise as bw


class TestMultiplexer4To1:
    def test_Multiplexer4To1(self):
        enable = bw.wire.Wire()
        select_1 = bw.wire.Wire()
        select_2 = bw.wire.Wire()
        input_1 = bw.wire.Wire()
        input_2 = bw.wire.Wire()
        input_3 = bw.wire.Wire()
        input_4 = bw.wire.Wire()
        output = bw.wire.Wire()
        bus_1 = bw.wire.Bus4(input_1, input_2, input_3, input_4)

        bw.signal.Multiplexer4To1(
            enable,
            select_1,
            select_2,
            bus_1,
            output
        )

        enable.value = 0
        select_1.value = 0
        select_2.value = 0
        input_1.value = 0
        input_2.value = 0
        input_3.value = 0
        input_4.value = 0
        assert output.value == 0

        enable.value = 0
        select_1.value = 1
        select_2.value = 1
        input_1.value = 1
        input_2.value = 1
        input_3.value = 1
        input_4.value = 1
        assert output.value == 0

        enable.value = 1
        select_1.value = 0
        select_2.value = 0
        input_1.value = 0
        input_2.value = 0
        input_3.value = 0
        input_4.value = 0
        assert output.value == 0

        enable.value = 1
        select_1.value = 0
        select_2.value = 0
        input_1.value = 0
        input_2.value = 0
        input_3.value = 0
        input_4.value = 1
        assert output.value == 1

        enable.value = 1
        select_1.value = 0
        select_2.value = 0
        input_1.value = 1
        input_2.value = 1
        input_3.value = 1
        input_4.value = 0
        assert output.value == 0

        enable.value = 1
        select_1.value = 0
        select_2.value = 0
        input_1.value = 1
        input_2.value = 1
        input_3.value = 1
        input_4.value = 1
        assert output.value == 1

        enable.value = 1
        select_1.value = 0
        select_2.value = 1
        input_1.value = 0
        input_2.value = 0
        input_3.value = 0
        input_4.value = 0
        assert output.value == 0

        enable.value = 1
        select_1.value = 0
        select_2.value = 1
        input_1.value = 0
        input_2.value = 0
        input_3.value = 1
        input_4.value = 0
        assert output.value == 1

        enable.value = 1
        select_1.value = 0
        select_2.value = 1
        input_1.value = 1
        input_2.value = 1
        input_3.value = 0
        input_4.value = 1
        assert output.value == 0

        enable.value = 1
        select_1.value = 0
        select_2.value = 1
        input_1.value = 1
        input_2.value = 1
        input_3.value = 1
        input_4.value = 1
        assert output.value == 1

        enable.value = 1
        select_1.value = 1
        select_2.value = 0
        input_1.value = 0
        input_2.value = 0
        input_3.value = 0
        input_4.value = 0
        assert output.value == 0

        enable.value = 1
        select_1.value = 1
        select_2.value = 0
        input_1.value = 0
        input_2.value = 1
        input_3.value = 0
        input_4.value = 0
        assert output.value == 1

        enable.value = 1
        select_1.value = 1
        select_2.value = 0
        input_1.value = 1
        input_2.value = 0
        input_3.value = 1
        input_4.value = 1
        assert output.value == 0

        enable.value = 1
        select_1.value = 1
        select_2.value = 0
        input_1.value = 1
        input_2.value = 1
        input_3.value = 1
        input_4.value = 1
        assert output.value == 1

        enable.value = 1
        select_1.value = 1
        select_2.value = 1
        input_1.value = 0
        input_2.value = 0
        input_3.value = 0
        input_4.value = 0
        assert output.value == 0

        enable.value = 1
        select_1.value = 1
        select_2.value = 1
        input_1.value = 1
        input_2.value = 0
        input_3.value = 0
        input_4.value = 0
        assert output.value == 1

        enable.value = 1
        select_1.value = 1
        select_2.value = 1
        input_1.value = 0
        input_2.value = 1
        input_3.value = 1
        input_4.value = 1
        assert output.value == 0

        enable.value = 1
        select_1.value = 1
        select_2.value = 1
        input_1.value = 1
        input_2.value = 1
        input_3.value = 1
        input_4.value = 1
        assert output.value == 1
