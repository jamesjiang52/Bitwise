import bitwise as bw


class TestShiftRegister16:
    def test_ShiftRegister16(self):
        enable = bw.wire.Wire()
        reset_n = bw.wire.Wire()
        shift_load = bw.wire.Wire()
        data_1 = bw.wire.Wire()
        data_2 = bw.wire.Wire()
        data_3 = bw.wire.Wire()
        data_4 = bw.wire.Wire()
        data_5 = bw.wire.Wire()
        data_6 = bw.wire.Wire()
        data_7 = bw.wire.Wire()
        data_8 = bw.wire.Wire()
        data_9 = bw.wire.Wire()
        data_10 = bw.wire.Wire()
        data_11 = bw.wire.Wire()
        data_12 = bw.wire.Wire()
        data_13 = bw.wire.Wire()
        data_14 = bw.wire.Wire()
        data_15 = bw.wire.Wire()
        data_16 = bw.wire.Wire()
        data_serial = bw.wire.Wire()
        clock = bw.wire.Wire()
        output_1 = bw.wire.Wire()
        output_2 = bw.wire.Wire()
        output_3 = bw.wire.Wire()
        output_4 = bw.wire.Wire()
        output_5 = bw.wire.Wire()
        output_6 = bw.wire.Wire()
        output_7 = bw.wire.Wire()
        output_8 = bw.wire.Wire()
        output_9 = bw.wire.Wire()
        output_10 = bw.wire.Wire()
        output_11 = bw.wire.Wire()
        output_12 = bw.wire.Wire()
        output_13 = bw.wire.Wire()
        output_14 = bw.wire.Wire()
        output_15 = bw.wire.Wire()
        output_16 = bw.wire.Wire()
        output_serial = bw.wire.Wire()
        data_bus = bw.wire.Bus16(
            data_1,
            data_2,
            data_3,
            data_4,
            data_5,
            data_6,
            data_7,
            data_8,
            data_9,
            data_10,
            data_11,
            data_12,
            data_13,
            data_14,
            data_15,
            data_16
        )
        output_bus = bw.wire.Bus16(
            output_1,
            output_2,
            output_3,
            output_4,
            output_5,
            output_6,
            output_7,
            output_8,
            output_9,
            output_10,
            output_11,
            output_12,
            output_13,
            output_14,
            output_15,
            output_16
        )

        bw.state.ShiftRegister16(
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
        assert output_bus.wire_values == (
            0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0)
        assert output_serial.value == 0

        data_1.value = 1
        data_2.value = 0
        data_3.value = 0
        data_4.value = 1
        data_5.value = 1
        data_6.value = 0
        data_7.value = 0
        data_8.value = 1
        data_9.value = 1
        data_10.value = 0
        data_11.value = 0
        data_12.value = 1
        data_13.value = 1
        data_14.value = 0
        data_15.value = 0
        data_16.value = 1
        data_serial.value = 1
        shift_load.value = 0
        clock.value = 0
        clock.value = 1
        shift_load.value = 1
        assert output_bus.wire_values == (
            1, 0, 0, 1, 1, 0, 0, 1, 1, 0, 0, 1, 1, 0, 0, 1)
        assert output_serial.value == 1

        clock.value = 0
        clock.value = 1
        assert output_bus.wire_values == (
            1, 1, 0, 0, 1, 1, 0, 0, 1, 1, 0, 0, 1, 1, 0, 0)
        assert output_serial.value == 0

        clock.value = 0
        clock.value = 1
        assert output_bus.wire_values == (
            1, 1, 1, 0, 0, 1, 1, 0, 0, 1, 1, 0, 0, 1, 1, 0)
        assert output_serial.value == 0

        reset_n.value = 0
        reset_n.value = 1
        assert output_bus.wire_values == (
            0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0)
        assert output_serial.value == 0

        data_1.value = 1
        data_2.value = 0
        data_3.value = 0
        data_4.value = 1
        data_5.value = 1
        data_6.value = 0
        data_7.value = 0
        data_8.value = 1
        data_9.value = 1
        data_10.value = 0
        data_11.value = 0
        data_12.value = 1
        data_13.value = 1
        data_14.value = 0
        data_15.value = 0
        data_16.value = 1
        data_serial.value = 0
        shift_load.value = 0
        clock.value = 0
        clock.value = 1
        shift_load.value = 1
        assert output_bus.wire_values == (
            1, 0, 0, 1, 1, 0, 0, 1, 1, 0, 0, 1, 1, 0, 0, 1)
        assert output_serial.value == 1

        clock.value = 0
        clock.value = 1
        assert output_bus.wire_values == (
            0, 1, 0, 0, 1, 1, 0, 0, 1, 1, 0, 0, 1, 1, 0, 0)
        assert output_serial.value == 0

        clock.value = 0
        clock.value = 1
        assert output_bus.wire_values == (
            0, 0, 1, 0, 0, 1, 1, 0, 0, 1, 1, 0, 0, 1, 1, 0)
        assert output_serial.value == 0

        enable.value = 0

        clock.value = 0
        clock.value = 1
        assert output_bus.wire_values == (
            0, 0, 1, 0, 0, 1, 1, 0, 0, 1, 1, 0, 0, 1, 1, 0)
        assert output_serial.value == 0

        enable.value = 1
        assert output_bus.wire_values == (
            0, 0, 1, 0, 0, 1, 1, 0, 0, 1, 1, 0, 0, 1, 1, 0)
        assert output_serial.value == 0

        clock.value = 0
        clock.value = 1
        assert output_bus.wire_values == (
            0, 0, 0, 1, 0, 0, 1, 1, 0, 0, 1, 1, 0, 0, 1, 1)
        assert output_serial.value == 1

        enable.value = 0
        clock.value = 0
        enable.value = 1
        assert output_bus.wire_values == (
            0, 0, 0, 1, 0, 0, 1, 1, 0, 0, 1, 1, 0, 0, 1, 1)
        assert output_serial.value == 1

        clock.value = 1
        assert output_bus.wire_values == (
            0, 0, 0, 0, 1, 0, 0, 1, 1, 0, 0, 1, 1, 0, 0, 1)
        assert output_serial.value == 1
