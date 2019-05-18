import bitwise as bw


class TestShiftRegister4:
    def test_ShiftRegister4(self):
        enable = bw.wire.Wire()
        reset_n = bw.wire.Wire()
        shift_load = bw.wire.Wire()
        data_1 = bw.wire.Wire()
        data_2 = bw.wire.Wire()
        data_3 = bw.wire.Wire()
        data_4 = bw.wire.Wire()
        data_serial = bw.wire.Wire()
        clock = bw.wire.Wire()
        output_1 = bw.wire.Wire()
        output_2 = bw.wire.Wire()
        output_3 = bw.wire.Wire()
        output_4 = bw.wire.Wire()
        output_serial = bw.wire.Wire()
        data_bus = bw.wire.Bus4(data_1, data_2, data_3, data_4)
        output_bus = bw.wire.Bus4(output_1, output_2, output_3, output_4)

        a = bw.state.ShiftRegister4(
            enable,
            reset_n,
            shift_load,
            data_bus,
            data_serial,
            clock,
            output_bus,
            output_serial
        )

        enable.value = 1

        reset_n.value = 0
        reset_n.value = 1
        assert output_bus.wire_values == (0, 0, 0, 0)
        assert output_serial.value == 0

        data_1.value = 1
        data_2.value = 0
        data_3.value = 0
        data_4.value = 1
        data_serial.value = 1
        shift_load.value = 0
        clock.value = 0
        clock.value = 1
        shift_load.value = 1
        assert output_bus.wire_values == (1, 0, 0, 1)
        assert output_serial.value == 1

        clock.value = 0
        clock.value = 1
        assert output_bus.wire_values == (1, 1, 0, 0)
        assert output_serial.value == 0

        clock.value = 0
        clock.value = 1
        assert output_bus.wire_values == (1, 1, 1, 0)
        assert output_serial.value == 0

        reset_n.value = 0
        reset_n.value = 1
        assert output_bus.wire_values == (0, 0, 0, 0)
        assert output_serial.value == 0

        data_1.value = 1
        data_2.value = 0
        data_3.value = 0
        data_4.value = 1
        data_serial.value = 0
        shift_load.value = 0
        clock.value = 0
        clock.value = 1
        shift_load.value = 1
        assert output_bus.wire_values == (1, 0, 0, 1)
        assert output_serial.value == 1

        clock.value = 0
        clock.value = 1
        assert output_bus.wire_values == (0, 1, 0, 0)
        assert output_serial.value == 0

        clock.value = 0
        clock.value = 1
        assert output_bus.wire_values == (0, 0, 1, 0)
        assert output_serial.value == 0

        enable.value = 0

        clock.value = 0
        clock.value = 1
        assert output_bus.wire_values == (0, 0, 1, 0)
        assert output_serial.value == 0

        enable.value = 1
        assert output_bus.wire_values == (0, 0, 1, 0)
        assert output_serial.value == 0

        clock.value = 0
        clock.value = 1
        assert output_bus.wire_values == (0, 0, 0, 1)
        assert output_serial.value == 1

        enable.value = 0
        clock.value = 0
        enable.value = 1
        assert output_bus.wire_values == (0, 0, 0, 1)
        assert output_serial.value == 1

        clock.value = 1
        assert output_bus.wire_values == (0, 0, 0, 0)
        assert output_serial.value == 0

        print(a.__doc__)
        print(a)

        a(
            enable=1,
            clear_n=1,
            shift_load=0,
            data_bus=(0, 1, 0, 1),
            data_serial=0,
            clock=0,
            output_bus=None,
            output_serial=None
        )
        assert output_bus.wire_values == (0, 0, 0, 0)
        assert output_serial.value == 0
        a(clock=1)
        assert output_bus.wire_values == (0, 1, 0, 1)
        assert output_serial.value == 1
