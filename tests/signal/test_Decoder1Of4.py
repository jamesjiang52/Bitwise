import bitwise as bw


class TestDecoder1Of4:
    def test_Decoder1Of4(self):
        enable = bw.wire.Wire()
        input_1 = bw.wire.Wire()
        input_2 = bw.wire.Wire()
        output_1 = bw.wire.Wire()
        output_2 = bw.wire.Wire()
        output_3 = bw.wire.Wire()
        output_4 = bw.wire.Wire()
        output_bus = bw.wire.Bus4(output_1, output_2, output_3, output_4)

        bw.signal.Decoder1Of4(enable, input_1, input_2, output_bus)

        enable.value = 0
        input_1.value = 0
        input_2.value = 0
        assert output_bus.wire_values == (0, 0, 0, 0)

        enable.value = 0
        input_1.value = 0
        input_2.value = 1
        assert output_bus.wire_values == (0, 0, 0, 0)

        enable.value = 0
        input_1.value = 1
        input_2.value = 0
        assert output_bus.wire_values == (0, 0, 0, 0)

        enable.value = 0
        input_1.value = 1
        input_2.value = 1
        assert output_bus.wire_values == (0, 0, 0, 0)

        enable.value = 1
        input_1.value = 0
        input_2.value = 0
        assert output_bus.wire_values == (0, 0, 0, 1)

        enable.value = 1
        input_1.value = 0
        input_2.value = 1
        assert output_bus.wire_values == (0, 0, 1, 0)

        enable.value = 1
        input_1.value = 1
        input_2.value = 0
        assert output_bus.wire_values == (0, 1, 0, 0)

        enable.value = 1
        input_1.value = 1
        input_2.value = 1
        assert output_bus.wire_values == (1, 0, 0, 0)
