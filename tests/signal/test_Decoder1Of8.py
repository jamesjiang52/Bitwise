import bitwise as bw


class TestDecoder1Of8:
    def test_Decoder1Of8(self):
        enable = bw.wire.Wire()
        input_1 = bw.wire.Wire()
        input_2 = bw.wire.Wire()
        input_3 = bw.wire.Wire()
        output_1 = bw.wire.Wire()
        output_2 = bw.wire.Wire()
        output_3 = bw.wire.Wire()
        output_4 = bw.wire.Wire()
        output_5 = bw.wire.Wire()
        output_6 = bw.wire.Wire()
        output_7 = bw.wire.Wire()
        output_8 = bw.wire.Wire()
        output_bus = bw.wire.Bus8(
            output_1,
            output_2,
            output_3,
            output_4,
            output_5,
            output_6,
            output_7,
            output_8
        )

        a = bw.signal.Decoder1Of8(
            enable, input_1, input_2, input_3, output_bus
        )

        enable.value = 0
        input_1.value = 0
        input_2.value = 0
        input_3.value = 0
        assert output_bus.wire_values == (0, 0, 0, 0, 0, 0, 0, 0)

        enable.value = 0
        input_1.value = 1
        input_2.value = 1
        input_3.value = 1
        assert output_bus.wire_values == (0, 0, 0, 0, 0, 0, 0, 0)

        enable.value = 1
        input_1.value = 0
        input_2.value = 0
        input_3.value = 0
        assert output_bus.wire_values == (0, 0, 0, 0, 0, 0, 0, 1)

        enable.value = 1
        input_1.value = 0
        input_2.value = 0
        input_3.value = 1
        assert output_bus.wire_values == (0, 0, 0, 0, 0, 0, 1, 0)

        enable.value = 1
        input_1.value = 0
        input_2.value = 1
        input_3.value = 0
        assert output_bus.wire_values == (0, 0, 0, 0, 0, 1, 0, 0)

        enable.value = 1
        input_1.value = 0
        input_2.value = 1
        input_3.value = 1
        assert output_bus.wire_values == (0, 0, 0, 0, 1, 0, 0, 0)

        enable.value = 1
        input_1.value = 1
        input_2.value = 0
        input_3.value = 0
        assert output_bus.wire_values == (0, 0, 0, 1, 0, 0, 0, 0)

        enable.value = 1
        input_1.value = 1
        input_2.value = 0
        input_3.value = 1
        assert output_bus.wire_values == (0, 0, 1, 0, 0, 0, 0, 0)

        enable.value = 1
        input_1.value = 1
        input_2.value = 1
        input_3.value = 0
        assert output_bus.wire_values == (0, 1, 0, 0, 0, 0, 0, 0)

        enable.value = 1
        input_1.value = 1
        input_2.value = 1
        input_3.value = 1
        assert output_bus.wire_values == (1, 0, 0, 0, 0, 0, 0, 0)

        print(a.__doc__)
        print(a)

        a(
            enable=1,
            input_1=1,
            input_2=0,
            input_3=0,
            output_bus=None
        )
        assert output_bus.wire_values == (0, 0, 0, 1, 0, 0, 0, 0)
