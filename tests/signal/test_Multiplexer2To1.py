import bitwise as bw


class TestMultiplexer2To1:
    def test_Multiplexer2To1(self):
        enable = bw.wire.Wire()
        select = bw.wire.Wire()
        input_1 = bw.wire.Wire()
        input_2 = bw.wire.Wire()
        output = bw.wire.Wire()

        bw.signal.Multiplexer2To1(enable, select, input_1, input_2, output)

        enable.value = 0
        select.value = 0
        input_1.value = 0
        input_2.value = 0
        assert output.value == 0

        enable.value = 0
        select.value = 1
        input_1.value = 1
        input_2.value = 1
        assert output.value == 0

        enable.value = 1
        select.value = 0
        input_1.value = 0
        input_2.value = 0
        assert output.value == 0

        enable.value = 1
        select.value = 0
        input_1.value = 0
        input_2.value = 1
        assert output.value == 1

        enable.value = 1
        select.value = 0
        input_1.value = 1
        input_2.value = 0
        assert output.value == 0

        enable.value = 1
        select.value = 0
        input_1.value = 1
        input_2.value = 1
        assert output.value == 1

        enable.value = 1
        select.value = 1
        input_1.value = 0
        input_2.value = 0
        assert output.value == 0

        enable.value = 1
        select.value = 1
        input_1.value = 0
        input_2.value = 1
        assert output.value == 0

        enable.value = 1
        select.value = 1
        input_1.value = 1
        input_2.value = 0
        assert output.value == 1

        enable.value = 1
        select.value = 1
        input_1.value = 1
        input_2.value = 1
        assert output.value == 1
