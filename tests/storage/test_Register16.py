import bitwise as bw


class TestRegister16:
    def test_Register16(self):
        input_1 = bw.wire.Wire()
        input_2 = bw.wire.Wire()
        input_3 = bw.wire.Wire()
        input_4 = bw.wire.Wire()
        input_5 = bw.wire.Wire()
        input_6 = bw.wire.Wire()
        input_7 = bw.wire.Wire()
        input_8 = bw.wire.Wire()
        input_9 = bw.wire.Wire()
        input_10 = bw.wire.Wire()
        input_11 = bw.wire.Wire()
        input_12 = bw.wire.Wire()
        input_13 = bw.wire.Wire()
        input_14 = bw.wire.Wire()
        input_15 = bw.wire.Wire()
        input_16 = bw.wire.Wire()
        enable = bw.wire.Wire()
        clock = bw.wire.Wire()
        input_bus = bw.wire.Bus16(
            input_1,
            input_2,
            input_3,
            input_4,
            input_5,
            input_6,
            input_7,
            input_8,
            input_9,
            input_10,
            input_11,
            input_12,
            input_13,
            input_14,
            input_15,
            input_16
        )
        output_bus = bw.wire.Bus16()

        a = bw.storage.Register16(input_bus, enable, clock, output_bus)

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
        input_9.value = 0
        input_10.value = 0
        input_11.value = 0
        input_12.value = 0
        input_13.value = 0
        input_14.value = 0
        input_15.value = 0
        input_16.value = 0
        clock.value = 1
        assert output_bus.wire_values == (
            0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0)

        clock.value = 0
        input_16.value = 1
        clock.value = 1
        assert output_bus.wire_values == (
            0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 1)

        clock.value = 0
        input_15.value = 1
        clock.value = 1
        assert output_bus.wire_values == (
            0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 1, 1)

        clock.value = 0
        input_14.value = 1
        clock.value = 1
        assert output_bus.wire_values == (
            0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 1, 1, 1)

        clock.value = 0
        input_13.value = 1
        clock.value = 1
        assert output_bus.wire_values == (
            0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 1, 1, 1, 1)

        clock.value = 0
        input_12.value = 1
        clock.value = 1
        assert output_bus.wire_values == (
            0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 1, 1, 1, 1, 1)

        clock.value = 0
        input_11.value = 1
        clock.value = 1
        assert output_bus.wire_values == (
            0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 1, 1, 1, 1, 1, 1)

        clock.value = 0
        input_10.value = 1
        clock.value = 1
        assert output_bus.wire_values == (
            0, 0, 0, 0, 0, 0, 0, 0, 0, 1, 1, 1, 1, 1, 1, 1)

        clock.value = 0
        input_9.value = 1
        clock.value = 1
        assert output_bus.wire_values == (
            0, 0, 0, 0, 0, 0, 0, 0, 1, 1, 1, 1, 1, 1, 1, 1)

        clock.value = 0
        input_8.value = 1
        clock.value = 1
        assert output_bus.wire_values == (
            0, 0, 0, 0, 0, 0, 0, 1, 1, 1, 1, 1, 1, 1, 1, 1)

        clock.value = 0
        input_7.value = 1
        clock.value = 1
        assert output_bus.wire_values == (
            0, 0, 0, 0, 0, 0, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1)

        clock.value = 0
        input_6.value = 1
        clock.value = 1
        assert output_bus.wire_values == (
            0, 0, 0, 0, 0, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1)

        clock.value = 0
        input_5.value = 1
        clock.value = 1
        assert output_bus.wire_values == (
            0, 0, 0, 0, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1)

        clock.value = 0
        input_4.value = 1
        clock.value = 1
        assert output_bus.wire_values == (
            0, 0, 0, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1)

        clock.value = 0
        input_3.value = 1
        clock.value = 1
        assert output_bus.wire_values == (
            0, 0, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1)

        clock.value = 0
        input_2.value = 1
        clock.value = 1
        assert output_bus.wire_values == (
            0, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1)

        enable.value = 0

        clock.value = 0
        input_1.value = 1
        clock.value = 1
        assert output_bus.wire_values == (
            0, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1)

        enable.value = 1

        clock.value = 0
        clock.value = 1
        assert output_bus.wire_values == (
            1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1)

        clock.value = 0
        input_1.value = 0
        input_2.value = 0
        input_3.value = 0
        input_4.value = 0
        input_5.value = 0
        input_6.value = 0
        input_7.value = 0
        input_8.value = 0
        input_9.value = 0
        input_10.value = 0
        input_11.value = 0
        input_12.value = 0
        input_13.value = 0
        input_14.value = 0
        input_15.value = 0
        input_16.value = 0
        clock.value = 1
        assert output_bus.wire_values == (
            0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0)

        print(a.__doc__)
        print(a)

        a(
            data_bus=(0, 0, 1, 1, 0, 0, 1, 1, 0, 0, 1, 1, 0, 0, 1, 1),
            enable=1,
            clock=0,
            output_bus=None
        )
        a(clock=1)
        assert output_bus.wire_values == (
            0, 0, 1, 1, 0, 0, 1, 1, 0, 0, 1, 1, 0, 0, 1, 1)
