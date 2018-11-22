import bitwise as bw


class TestRegister8:
    def test_Register8(self):
        input_1 = bw.wire.Wire()
        input_2 = bw.wire.Wire()
        input_3 = bw.wire.Wire()
        input_4 = bw.wire.Wire()
        input_5 = bw.wire.Wire()
        input_6 = bw.wire.Wire()
        input_7 = bw.wire.Wire()
        input_8 = bw.wire.Wire()
        enable = bw.wire.Wire()
        clock = bw.wire.Wire()
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

        bw.storage.Register8(input_bus, enable, clock, output_bus)

        enable.value = 1

        clock.value = 0
        input_1.value = 0
        input_2.value = 0
        input_3.value = 0
        input_4.value = 0
        input_5.value = 0
        input_6.value = 0
        input_7.value = 0
        input_8.value = 0
        clock.value = 1
        assert output_bus.wire_values == (0, 0, 0, 0, 0, 0, 0, 0)

        clock.value = 0
        input_8.value = 1
        clock.value = 1
        assert output_bus.wire_values == (0, 0, 0, 0, 0, 0, 0, 1)

        clock.value = 0
        input_7.value = 1
        clock.value = 1
        assert output_bus.wire_values == (0, 0, 0, 0, 0, 0, 1, 1)

        clock.value = 0
        input_6.value = 1
        clock.value = 1
        assert output_bus.wire_values == (0, 0, 0, 0, 0, 1, 1, 1)

        clock.value = 0
        input_5.value = 1
        clock.value = 1
        assert output_bus.wire_values == (0, 0, 0, 0, 1, 1, 1, 1)

        clock.value = 0
        input_4.value = 1
        clock.value = 1
        assert output_bus.wire_values == (0, 0, 0, 1, 1, 1, 1, 1)

        clock.value = 0
        input_3.value = 1
        clock.value = 1
        assert output_bus.wire_values == (0, 0, 1, 1, 1, 1, 1, 1)

        clock.value = 0
        input_2.value = 1
        clock.value = 1
        assert output_bus.wire_values == (0, 1, 1, 1, 1, 1, 1, 1)

        enable.value = 0

        clock.value = 0
        input_1.value = 1
        clock.value = 1
        assert output_bus.wire_values == (0, 1, 1, 1, 1, 1, 1, 1)

        enable.value = 1

        clock.value = 0
        clock.value = 1
        assert output_bus.wire_values == (1, 1, 1, 1, 1, 1, 1, 1)

        clock.value = 0
        input_1.value = 0
        input_2.value = 0
        input_3.value = 0
        input_4.value = 0
        input_5.value = 0
        input_6.value = 0
        input_7.value = 0
        input_8.value = 0
        clock.value = 1
        assert output_bus.wire_values == (0, 0, 0, 0, 0, 0, 0, 0)
