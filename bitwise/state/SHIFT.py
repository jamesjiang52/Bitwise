"""
The following classes are defined:
    ShiftRegister4
    ShiftRegister8
    ShiftRegister16
"""

from .. import wire
from .. import gate
from .. import signal
from .. import storage

Wire = wire.Wire
Bus4 = wire.Bus4
Bus8 = wire.Bus8
Bus16 = wire.Bus16


class ShiftRegister4:
    """Construct a new 4-bit shift register.

    Args:
        enable: An object of type Wire. Enables the shift register.
        clear_n: An object of type Wire. Clears output_bus and output_serial to
            0 asynchronously if its value is 0.
        shift_load: An object of type Wire. The mode select. A value of 0
            indicates a parallel load operation, where output_bus takes on the
            value of data_bus. A value of 1 indicates a shift-right operation,
            where output_bus[3] takes on the value of output_bus[2],
            output_bus[2] takes on the value of output_bus[1], and so on;
            output_bus[0] takes on the value of data_serial.
        data_bus: An object of type Bus4. The parallel data input.
        data_serial. An object of type Wire. The serial data input.
        clock. An object of type Wire or Clock. The clock input to the shift
            register.
        output_bus. An object of type Bus4. The parallel data output.
        output_serial. An object of type Wire. The serial data output.
            Identical to output_bus[3].

    Raises:
        TypeError: If either data_bus or output_bus is not a bus of width 4.
    """
    def __init__(
        self,
        enable,
        reset_n,
        shift_load,
        data_bus,
        data_s,
        clock,
        output_bus,
        output_s
    ):
        if len(data_bus) != 4:
            raise TypeError(
                "Expected bus of width 4, received bus of width {0}.".format(
                    len(data_bus)
                )
            )

        if len(output_bus) != 4:
            raise TypeError(
                "Expected bus of width 4, received bus of width {0}.".format(
                    len(output_bus)
                )
            )

        gnd = Wire()
        gnd.value = 0
        vcc = Wire()
        vcc.value = 1

        mux_1_out = Wire()
        mux_2_out = Wire()
        mux_3_out = Wire()
        mux_4_out = Wire()
        q_1_not = Wire()
        q_2_not = Wire()
        q_3_not = Wire()
        q_4_not = Wire()

        mux_1 = Bus4(data_s, data_bus[0], output_bus[0], output_bus[0])
        mux_2 = Bus4(output_bus[0], data_bus[1], output_bus[1], output_bus[1])
        mux_3 = Bus4(output_bus[1], data_bus[2], output_bus[2], output_bus[2])
        mux_4 = Bus4(output_bus[2], data_bus[3], output_bus[3], output_bus[3])

        signal.Multiplexer4To1(vcc, enable, shift_load, mux_1, mux_1_out)
        signal.Multiplexer4To1(vcc, enable, shift_load, mux_2, mux_2_out)
        signal.Multiplexer4To1(vcc, enable, shift_load, mux_3, mux_3_out)
        signal.Multiplexer4To1(vcc, enable, shift_load, mux_4, mux_4_out)

        storage.DFlipFlopPresetClear(
            mux_4_out,
            vcc,
            reset_n,
            clock,
            output_bus[3],
            q_4_not
        )
        storage.DFlipFlopPresetClear(
            mux_3_out,
            vcc,
            reset_n,
            clock,
            output_bus[2],
            q_3_not
        )
        storage.DFlipFlopPresetClear(
            mux_2_out,
            vcc,
            reset_n,
            clock,
            output_bus[1],
            q_2_not
        )
        storage.DFlipFlopPresetClear(
            mux_1_out,
            vcc,
            reset_n,
            clock,
            output_bus[0],
            q_1_not
        )
        gate.Buffer(output_bus[3], output_s)

        self.enable = enable
        self.clear_n = reset_n
        self.shift_load = shift_load
        self.data_bus = data_bus
        self.data_serial = data_s
        self.clock = clock
        self.output_bus = output_bus
        self.output_serial = output_s

    def __str__(self):
        str_ = ""
        str_ += "enable: " + str(self.enable.value) + "\n"
        str_ += "clear_n: " + str(self.clear_n.value) + "\n"
        str_ += "shift_load: " + str(self.shift_load.value) + "\n"
        str_ += "data_bus: " + self.data_bus.__str__() + "\n"
        str_ += "data_serial: " + str(self.data_serial.value) + "\n"
        str_ += "clock: " + str(self.clock.value) + "\n"
        str_ += "output_bus: " + self.output_bus.__str__() + "\n"
        str_ += "output_serial: " + str(self.output_serial.value)
        return str_

    def __call__(
        self, *,
        enable=None,
        clear_n=None,
        shift_load=None,
        data_bus=None,
        data_serial=None,
        clock=None,
        output_bus=None,
        output_serial=None
    ):
        if enable is not None:
            self.enable.value = enable
        if clear_n is not None:
            self.clear_n.value = clear_n
        if shift_load is not None:
            self.shift_load.value = shift_load
        if data_bus is not None:
            self.data_bus.wire_values = data_bus
        if data_serial is not None:
            self.data_serial.value = data_serial
        if clock is not None:
            self.clock.value = clock
        if output_bus is not None:
            self.output_bus.wire_values = output_bus
        if output_serial is not None:
            self.output_serial.value = output_serial


class ShiftRegister8:
    """Construct a new 8-bit shift register.

    Args:
        enable: An object of type Wire. Enables the shift register.
        clear_n: An object of type Wire. Clears output_bus and output_serial to
            0 asynchronously if its value is 0.
        shift_load: An object of type Wire. The mode select. A value of 0
            indicates a parallel load operation, where output_bus takes on the
            value of data_bus. A value of 1 indicates a shift-right operation,
            where output_bus[7] takes on the value of output_bus[6],
            output_bus[6] takes on the value of output_bus[5], and so on;
            output_bus[0] takes on the value of data_serial.
        data_bus: An object of type Bus8. The parallel data input.
        data_serial. An object of type Wire. The serial data input.
        clock. An object of type Wire or Clock. The clock input to the shift
            register.
        output_bus. An object of type Bus8. The parallel data output.
        output_serial. An object of type Wire. The serial data output.
            Identical to output_bus[7].

    Raises:
        TypeError: If either data_bus or output_bus is not a bus of width 8.
    """
    def __init__(
        self,
        enable,
        reset_n,
        shift_load,
        data_bus,
        data_s,
        clock,
        output_bus,
        output_s
    ):
        if len(data_bus) != 8:
            raise TypeError(
                "Expected bus of width 8, received bus of width {0}.".format(
                    len(data_bus)
                )
            )

        if len(output_bus) != 8:
            raise TypeError(
                "Expected bus of width 8, received bus of width {0}.".format(
                    len(output_bus)
                )
            )

        gnd = Wire()
        gnd.value = 0
        vcc = Wire()
        vcc.value = 1

        mux_1_out = Wire()
        mux_2_out = Wire()
        mux_3_out = Wire()
        mux_4_out = Wire()
        mux_5_out = Wire()
        mux_6_out = Wire()
        mux_7_out = Wire()
        mux_8_out = Wire()
        q_1_not = Wire()
        q_2_not = Wire()
        q_3_not = Wire()
        q_4_not = Wire()
        q_5_not = Wire()
        q_6_not = Wire()
        q_7_not = Wire()
        q_8_not = Wire()

        mux_1 = Bus4(data_s, data_bus[0], output_bus[0], output_bus[0])
        mux_2 = Bus4(output_bus[0], data_bus[1], output_bus[1], output_bus[1])
        mux_3 = Bus4(output_bus[1], data_bus[2], output_bus[2], output_bus[2])
        mux_4 = Bus4(output_bus[2], data_bus[3], output_bus[3], output_bus[3])
        mux_5 = Bus4(output_bus[3], data_bus[4], output_bus[4], output_bus[4])
        mux_6 = Bus4(output_bus[4], data_bus[5], output_bus[5], output_bus[5])
        mux_7 = Bus4(output_bus[5], data_bus[6], output_bus[6], output_bus[6])
        mux_8 = Bus4(output_bus[6], data_bus[7], output_bus[7], output_bus[7])

        signal.Multiplexer4To1(vcc, enable, shift_load, mux_1, mux_1_out)
        signal.Multiplexer4To1(vcc, enable, shift_load, mux_2, mux_2_out)
        signal.Multiplexer4To1(vcc, enable, shift_load, mux_3, mux_3_out)
        signal.Multiplexer4To1(vcc, enable, shift_load, mux_4, mux_4_out)
        signal.Multiplexer4To1(vcc, enable, shift_load, mux_5, mux_5_out)
        signal.Multiplexer4To1(vcc, enable, shift_load, mux_6, mux_6_out)
        signal.Multiplexer4To1(vcc, enable, shift_load, mux_7, mux_7_out)
        signal.Multiplexer4To1(vcc, enable, shift_load, mux_8, mux_8_out)

        storage.DFlipFlopPresetClear(
            mux_8_out,
            vcc,
            reset_n,
            clock,
            output_bus[7],
            q_8_not
        )
        storage.DFlipFlopPresetClear(
            mux_7_out,
            vcc,
            reset_n,
            clock,
            output_bus[6],
            q_7_not
        )
        storage.DFlipFlopPresetClear(
            mux_6_out,
            vcc,
            reset_n,
            clock,
            output_bus[5],
            q_6_not
        )
        storage.DFlipFlopPresetClear(
            mux_5_out,
            vcc,
            reset_n,
            clock,
            output_bus[4],
            q_5_not
        )
        storage.DFlipFlopPresetClear(
            mux_4_out,
            vcc,
            reset_n,
            clock,
            output_bus[3],
            q_4_not
        )
        storage.DFlipFlopPresetClear(
            mux_3_out,
            vcc,
            reset_n,
            clock,
            output_bus[2],
            q_3_not
        )
        storage.DFlipFlopPresetClear(
            mux_2_out,
            vcc,
            reset_n,
            clock,
            output_bus[1],
            q_2_not
        )
        storage.DFlipFlopPresetClear(
            mux_1_out,
            vcc,
            reset_n,
            clock,
            output_bus[0],
            q_1_not
        )
        gate.Buffer(output_bus[7], output_s)

        self.enable = enable
        self.clear_n = reset_n
        self.shift_load = shift_load
        self.data_bus = data_bus
        self.data_serial = data_s
        self.clock = clock
        self.output_bus = output_bus
        self.output_serial = output_s

    def __str__(self):
        str_ = ""
        str_ += "enable: " + str(self.enable.value) + "\n"
        str_ += "clear_n: " + str(self.clear_n.value) + "\n"
        str_ += "shift_load: " + str(self.shift_load.value) + "\n"
        str_ += "data_bus: " + self.data_bus.__str__() + "\n"
        str_ += "data_serial: " + str(self.data_serial.value) + "\n"
        str_ += "clock: " + str(self.clock.value) + "\n"
        str_ += "output_bus: " + self.output_bus.__str__() + "\n"
        str_ += "output_serial: " + str(self.output_serial.value)
        return str_

    def __call__(
        self, *,
        enable=None,
        clear_n=None,
        shift_load=None,
        data_bus=None,
        data_serial=None,
        clock=None,
        output_bus=None,
        output_serial=None
    ):
        if enable is not None:
            self.enable.value = enable
        if clear_n is not None:
            self.clear_n.value = clear_n
        if shift_load is not None:
            self.shift_load.value = shift_load
        if data_bus is not None:
            self.data_bus.wire_values = data_bus
        if data_serial is not None:
            self.data_serial.value = data_serial
        if clock is not None:
            self.clock.value = clock
        if output_bus is not None:
            self.output_bus.wire_values = output_bus
        if output_serial is not None:
            self.output_serial.value = output_serial


class ShiftRegister16:
    """Construct a new 16-bit shift register.

    Args:
        enable: An object of type Wire. Enables the shift register.
        clear_n: An object of type Wire. Clears output_bus and output_serial to
            0 asynchronously if its value is 0.
        shift_load: An object of type Wire. The mode select. A value of 0
            indicates a parallel load operation, where output_bus takes on the
            value of data_bus. A value of 1 indicates a shift-right operation,
            where output_bus[15] takes on the value of output_bus[14],
            output_bus[14] takes on the value of output_bus[13], and so on;
            output_bus[0] takes on the value of data_serial.
        data_bus: An object of type Bus16. The parallel data input.
        data_serial. An object of type Wire. The serial data input.
        clock. An object of type Wire or Clock. The clock input to the shift
            register.
        output_bus. An object of type Bus16. The parallel data output.
        output_serial. An object of type Wire. The serial data output.
            Identical to output_bus[15].

    Raises:
        TypeError: If either data_bus or output_bus is not a bus of width 16.
    """
    def __init__(
        self,
        enable,
        reset_n,
        shift_load,
        data_bus,
        data_s,
        clock,
        output_bus,
        output_s
    ):
        if len(data_bus) != 16:
            raise TypeError(
                "Expected bus of width 16, received bus of width {0}.".format(
                    len(data_bus)
                )
            )

        if len(output_bus) != 16:
            raise TypeError(
                "Expected bus of width 16, received bus of width {0}.".format(
                    len(output_bus)
                )
            )

        gnd = Wire()
        gnd.value = 0
        vcc = Wire()
        vcc.value = 1

        mux_1_out = Wire()
        mux_2_out = Wire()
        mux_3_out = Wire()
        mux_4_out = Wire()
        mux_5_out = Wire()
        mux_6_out = Wire()
        mux_7_out = Wire()
        mux_8_out = Wire()
        mux_9_out = Wire()
        mux_10_out = Wire()
        mux_11_out = Wire()
        mux_12_out = Wire()
        mux_13_out = Wire()
        mux_14_out = Wire()
        mux_15_out = Wire()
        mux_16_out = Wire()
        q_1_not = Wire()
        q_2_not = Wire()
        q_3_not = Wire()
        q_4_not = Wire()
        q_5_not = Wire()
        q_6_not = Wire()
        q_7_not = Wire()
        q_8_not = Wire()
        q_9_not = Wire()
        q_10_not = Wire()
        q_11_not = Wire()
        q_12_not = Wire()
        q_13_not = Wire()
        q_14_not = Wire()
        q_15_not = Wire()
        q_16_not = Wire()

        o_bus = output_bus

        mux_1 = Bus4(data_s, data_bus[0], o_bus[0], o_bus[0])
        mux_2 = Bus4(o_bus[0], data_bus[1], o_bus[1], o_bus[1])
        mux_3 = Bus4(o_bus[1], data_bus[2], o_bus[2], o_bus[2])
        mux_4 = Bus4(o_bus[2], data_bus[3], o_bus[3], o_bus[3])
        mux_5 = Bus4(o_bus[3], data_bus[4], o_bus[4], o_bus[4])
        mux_6 = Bus4(o_bus[4], data_bus[5], o_bus[5], o_bus[5])
        mux_7 = Bus4(o_bus[5], data_bus[6], o_bus[6], o_bus[6])
        mux_8 = Bus4(o_bus[6], data_bus[7], o_bus[7], o_bus[7])
        mux_9 = Bus4(o_bus[7], data_bus[8], o_bus[8], o_bus[8])
        mux_10 = Bus4(o_bus[8], data_bus[9], o_bus[9], o_bus[9])
        mux_11 = Bus4(o_bus[9], data_bus[10], o_bus[10], o_bus[10])
        mux_12 = Bus4(o_bus[10], data_bus[11], o_bus[11], o_bus[11])
        mux_13 = Bus4(o_bus[11], data_bus[12], o_bus[12], o_bus[12])
        mux_14 = Bus4(o_bus[12], data_bus[13], o_bus[13], o_bus[13])
        mux_15 = Bus4(o_bus[13], data_bus[14], o_bus[14], o_bus[14])
        mux_16 = Bus4(o_bus[14], data_bus[15], o_bus[15], o_bus[15])

        signal.Multiplexer4To1(vcc, enable, shift_load, mux_1, mux_1_out)
        signal.Multiplexer4To1(vcc, enable, shift_load, mux_2, mux_2_out)
        signal.Multiplexer4To1(vcc, enable, shift_load, mux_3, mux_3_out)
        signal.Multiplexer4To1(vcc, enable, shift_load, mux_4, mux_4_out)
        signal.Multiplexer4To1(vcc, enable, shift_load, mux_5, mux_5_out)
        signal.Multiplexer4To1(vcc, enable, shift_load, mux_6, mux_6_out)
        signal.Multiplexer4To1(vcc, enable, shift_load, mux_7, mux_7_out)
        signal.Multiplexer4To1(vcc, enable, shift_load, mux_8, mux_8_out)
        signal.Multiplexer4To1(vcc, enable, shift_load, mux_9, mux_9_out)
        signal.Multiplexer4To1(vcc, enable, shift_load, mux_10, mux_10_out)
        signal.Multiplexer4To1(vcc, enable, shift_load, mux_11, mux_11_out)
        signal.Multiplexer4To1(vcc, enable, shift_load, mux_12, mux_12_out)
        signal.Multiplexer4To1(vcc, enable, shift_load, mux_13, mux_13_out)
        signal.Multiplexer4To1(vcc, enable, shift_load, mux_14, mux_14_out)
        signal.Multiplexer4To1(vcc, enable, shift_load, mux_15, mux_15_out)
        signal.Multiplexer4To1(vcc, enable, shift_load, mux_16, mux_16_out)

        storage.DFlipFlopPresetClear(
            mux_16_out,
            vcc,
            reset_n,
            clock,
            output_bus[15],
            q_16_not
        )
        storage.DFlipFlopPresetClear(
            mux_15_out,
            vcc,
            reset_n,
            clock,
            output_bus[14],
            q_15_not
        )
        storage.DFlipFlopPresetClear(
            mux_14_out,
            vcc,
            reset_n,
            clock,
            output_bus[13],
            q_14_not
        )
        storage.DFlipFlopPresetClear(
            mux_13_out,
            vcc,
            reset_n,
            clock,
            output_bus[12],
            q_13_not
        )
        storage.DFlipFlopPresetClear(
            mux_12_out,
            vcc,
            reset_n,
            clock,
            output_bus[11],
            q_12_not
        )
        storage.DFlipFlopPresetClear(
            mux_11_out,
            vcc,
            reset_n,
            clock,
            output_bus[10],
            q_11_not
        )
        storage.DFlipFlopPresetClear(
            mux_10_out,
            vcc,
            reset_n,
            clock,
            output_bus[9],
            q_10_not
        )
        storage.DFlipFlopPresetClear(
            mux_9_out,
            vcc,
            reset_n,
            clock,
            output_bus[8],
            q_9_not
        )
        storage.DFlipFlopPresetClear(
            mux_8_out,
            vcc,
            reset_n,
            clock,
            output_bus[7],
            q_8_not
        )
        storage.DFlipFlopPresetClear(
            mux_7_out,
            vcc,
            reset_n,
            clock,
            output_bus[6],
            q_7_not
        )
        storage.DFlipFlopPresetClear(
            mux_6_out,
            vcc,
            reset_n,
            clock,
            output_bus[5],
            q_6_not
        )
        storage.DFlipFlopPresetClear(
            mux_5_out,
            vcc,
            reset_n,
            clock,
            output_bus[4],
            q_5_not
        )
        storage.DFlipFlopPresetClear(
            mux_4_out,
            vcc,
            reset_n,
            clock,
            output_bus[3],
            q_4_not
        )
        storage.DFlipFlopPresetClear(
            mux_3_out,
            vcc,
            reset_n,
            clock,
            output_bus[2],
            q_3_not
        )
        storage.DFlipFlopPresetClear(
            mux_2_out,
            vcc,
            reset_n,
            clock,
            output_bus[1],
            q_2_not
        )
        storage.DFlipFlopPresetClear(
            mux_1_out,
            vcc,
            reset_n,
            clock,
            output_bus[0],
            q_1_not
        )
        gate.Buffer(output_bus[15], output_s)

        self.enable = enable
        self.clear_n = reset_n
        self.shift_load = shift_load
        self.data_bus = data_bus
        self.data_serial = data_s
        self.clock = clock
        self.output_bus = output_bus
        self.output_serial = output_s

    def __str__(self):
        str_ = ""
        str_ += "enable: " + str(self.enable.value) + "\n"
        str_ += "clear_n: " + str(self.clear_n.value) + "\n"
        str_ += "shift_load: " + str(self.shift_load.value) + "\n"
        str_ += "data_bus: " + self.data_bus.__str__() + "\n"
        str_ += "data_serial: " + str(self.data_serial.value) + "\n"
        str_ += "clock: " + str(self.clock.value) + "\n"
        str_ += "output_bus: " + self.output_bus.__str__() + "\n"
        str_ += "output_serial: " + str(self.output_serial.value)
        return str_

    def __call__(
        self, *,
        enable=None,
        clear_n=None,
        shift_load=None,
        data_bus=None,
        data_serial=None,
        clock=None,
        output_bus=None,
        output_serial=None
    ):
        if enable is not None:
            self.enable.value = enable
        if clear_n is not None:
            self.clear_n.value = clear_n
        if shift_load is not None:
            self.shift_load.value = shift_load
        if data_bus is not None:
            self.data_bus.wire_values = data_bus
        if data_serial is not None:
            self.data_serial.value = data_serial
        if clock is not None:
            self.clock.value = clock
        if output_bus is not None:
            self.output_bus.wire_values = output_bus
        if output_serial is not None:
            self.output_serial.value = output_serial
