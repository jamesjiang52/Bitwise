import bitwise as bw


class TestBufferBus8:
    def test_BufferBus8(self):
        enable = bw.wire.Wire()
        input_1 = bw.wire.Wire()
        input_2 = bw.wire.Wire()
        input_3 = bw.wire.Wire()
        input_4 = bw.wire.Wire()
        input_5 = bw.wire.Wire()
        input_6 = bw.wire.Wire()
        input_7 = bw.wire.Wire()
        input_8 = bw.wire.Wire()
        output_1 = bw.wire.Wire()
        output_2 = bw.wire.Wire()
        output_3 = bw.wire.Wire()
        output_4 = bw.wire.Wire()
        output_5 = bw.wire.Wire()
        output_6 = bw.wire.Wire()
        output_7 = bw.wire.Wire()
        output_8 = bw.wire.Wire()
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

        a = bw.wire.BufferBus8(enable, input_bus, output_bus)

        enable.value = 0
        input_1.value = 0
        input_2.value = 0
        input_3.value = 0
        input_4.value = 0
        input_5.value = 0
        input_6.value = 0
        input_7.value = 0
        input_8.value = 0
        assert output_bus.wire_values == (0, 0, 0, 0, 0, 0, 0, 0)

        input_1.value = 0
        input_2.value = 1
        input_3.value = 0
        input_4.value = 1
        input_5.value = 0
        input_6.value = 1
        input_7.value = 0
        input_8.value = 1
        assert output_bus.wire_values == (0, 0, 0, 0, 0, 0, 0, 0)

        enable.value = 1
        assert output_bus.wire_values == (0, 1, 0, 1, 0, 1, 0, 1)

        input_1.value = 1
        input_2.value = 1
        input_3.value = 1
        input_4.value = 1
        input_5.value = 0
        input_6.value = 0
        input_7.value = 0
        input_8.value = 0
        assert output_bus.wire_values == (1, 1, 1, 1, 0, 0, 0, 0)

        enable.value = 0
        assert output_bus.wire_values == (1, 1, 1, 1, 0, 0, 0, 0)

        input_1.value = 0
        input_2.value = 0
        input_3.value = 0
        input_4.value = 0
        input_5.value = 1
        input_6.value = 1
        input_7.value = 1
        input_8.value = 1
        assert output_bus.wire_values == (1, 1, 1, 1, 0, 0, 0, 0)

        output_1.value = 0
        output_2.value = 1
        output_3.value = 0
        output_4.value = 1
        output_5.value = 0
        output_6.value = 1
        output_7.value = 0
        output_8.value = 1
        assert input_bus.wire_values == (0, 0, 0, 0, 1, 1, 1, 1)

        print(a.__doc__)
        print(a)

        a(enable=1, input_bus=(0, 0, 0, 0, 0, 0, 0, 0), output_bus=None)
        assert output_bus.wire_values == (0, 0, 0, 0, 0, 0, 0, 0)
