"""
The following classes are defined:
    Register4
    Register8
    Register16
"""

from .. import wire
from .. import signal
from . import FLOP

Wire = wire.Wire
Bus4 = wire.Bus4
Bus8 = wire.Bus8
Bus16 = wire.Bus16


class Register4:
    """Construct a new 4-bit storage register.

    Args:
        data_bus: An object of type Bus4. The data input to the register.
        enable: An object of type Wire. Enables the register.
        clock: An object of type Wire or Clock. The clock input to the
            register.
        output_bus: An object of type Bus4. The output of the register. Takes
            on the value of data_bus on the positive edges of clock if the
            value of enable is 1.

    Raises:
        TypeError: If either data_bus or output_bus is not a bus of width 4.
    """
    def __init__(self, data_bus, enable, clock, output_bus):
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

        not_1 = Wire()
        not_2 = Wire()
        not_3 = Wire()
        not_4 = Wire()

        mux_bus = Bus4()

        _Multiplexer2To1_4(enable, data_bus, output_bus, mux_bus)

        FLOP.DFlipFlop(mux_bus[0], clock, output_bus[0], not_1)
        FLOP.DFlipFlop(mux_bus[1], clock, output_bus[1], not_2)
        FLOP.DFlipFlop(mux_bus[2], clock, output_bus[2], not_3)
        FLOP.DFlipFlop(mux_bus[3], clock, output_bus[3], not_4)

        self.data_bus = data_bus
        self.enable = enable
        self.clock = clock
        self.output_bus = output_bus

    def __str__(self):
        str_ = ""
        str_ += "data_bus: " + self.data_bus.__str__() + "\n"
        str_ += "enable: " + str(self.enable.value) + "\n"
        str_ += "clock: " + str(self.clock.value) + "\n"
        str_ += "output_bus: " + self.output_bus.__str__()
        return str_

    def __call__(
        self, *,
        data_bus=None,
        enable=None,
        clock=None,
        output_bus=None
    ):
        if data_bus is not None:
            self.data_bus.wire_values = data_bus
        if enable is not None:
            self.enable.value = enable
        if clock is not None:
            self.clock.value = clock
        if output_bus is not None:
            self.output_bus.wire_values = output_bus


class Register8:
    """Construct a new 8-bit storage register.

    Args:
        data_bus: An object of type Bus8. The data input to the register.
        enable: An object of type Wire. Enables the register.
        clock: An object of type Wire or Clock. The clock input to the
            register.
        output_bus: An object of type Bus8. The output of the register. Takes
            on the value of data_bus on the positive edges of clock if the
            value of enable is 1.

    Raises:
        TypeError: If either data_bus or output_bus is not a bus of width 8.
    """
    def __init__(self, data_bus, enable, clock, output_bus):
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

        not_1 = Wire()
        not_2 = Wire()
        not_3 = Wire()
        not_4 = Wire()
        not_5 = Wire()
        not_6 = Wire()
        not_7 = Wire()
        not_8 = Wire()

        mux_bus = Bus8()

        _Multiplexer2To1_8(enable, data_bus, output_bus, mux_bus)

        FLOP.DFlipFlop(mux_bus[0], clock, output_bus[0], not_1)
        FLOP.DFlipFlop(mux_bus[1], clock, output_bus[1], not_2)
        FLOP.DFlipFlop(mux_bus[2], clock, output_bus[2], not_3)
        FLOP.DFlipFlop(mux_bus[3], clock, output_bus[3], not_4)
        FLOP.DFlipFlop(mux_bus[4], clock, output_bus[4], not_5)
        FLOP.DFlipFlop(mux_bus[5], clock, output_bus[5], not_6)
        FLOP.DFlipFlop(mux_bus[6], clock, output_bus[6], not_7)
        FLOP.DFlipFlop(mux_bus[7], clock, output_bus[7], not_8)

        self.data_bus = data_bus
        self.enable = enable
        self.clock = clock
        self.output_bus = output_bus

    def __str__(self):
        str_ = ""
        str_ += "data_bus: " + self.data_bus.__str__() + "\n"
        str_ += "enable: " + str(self.enable.value) + "\n"
        str_ += "clock: " + str(self.clock.value) + "\n"
        str_ += "output_bus: " + self.output_bus.__str__()
        return str_

    def __call__(
        self, *,
        data_bus=None,
        enable=None,
        clock=None,
        output_bus=None
    ):
        if data_bus is not None:
            self.data_bus.wire_values = data_bus
        if enable is not None:
            self.enable.value = enable
        if clock is not None:
            self.clock.value = clock
        if output_bus is not None:
            self.output_bus.wire_values = output_bus


class Register16:
    """Construct a new 16-bit storage register.

    Args:
        data_bus: An object of type Bus16. The data input to the register.
        enable: An object of type Wire. Enables the register.
        clock: An object of type Wire or Clock. The clock input to the
            register.
        output_bus: An object of type Bus16. The output of the register. Takes
            on the value of data_bus on the positive edges of clock if the
            value of enable is 1.

    Raises:
        TypeError: If either data_bus or output_bus is not a bus of width 16.
    """
    def __init__(self, data_bus, enable, clock, output_bus):
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

        not_1 = Wire()
        not_2 = Wire()
        not_3 = Wire()
        not_4 = Wire()
        not_5 = Wire()
        not_6 = Wire()
        not_7 = Wire()
        not_8 = Wire()
        not_9 = Wire()
        not_10 = Wire()
        not_11 = Wire()
        not_12 = Wire()
        not_13 = Wire()
        not_14 = Wire()
        not_15 = Wire()
        not_16 = Wire()

        mux_bus = Bus16()

        _Multiplexer2To1_16(enable, data_bus, output_bus, mux_bus)

        FLOP.DFlipFlop(mux_bus[0], clock, output_bus[0], not_1)
        FLOP.DFlipFlop(mux_bus[1], clock, output_bus[1], not_2)
        FLOP.DFlipFlop(mux_bus[2], clock, output_bus[2], not_3)
        FLOP.DFlipFlop(mux_bus[3], clock, output_bus[3], not_4)
        FLOP.DFlipFlop(mux_bus[4], clock, output_bus[4], not_5)
        FLOP.DFlipFlop(mux_bus[5], clock, output_bus[5], not_6)
        FLOP.DFlipFlop(mux_bus[6], clock, output_bus[6], not_7)
        FLOP.DFlipFlop(mux_bus[7], clock, output_bus[7], not_8)
        FLOP.DFlipFlop(mux_bus[8], clock, output_bus[8], not_9)
        FLOP.DFlipFlop(mux_bus[9], clock, output_bus[9], not_10)
        FLOP.DFlipFlop(mux_bus[10], clock, output_bus[10], not_11)
        FLOP.DFlipFlop(mux_bus[11], clock, output_bus[11], not_12)
        FLOP.DFlipFlop(mux_bus[12], clock, output_bus[12], not_13)
        FLOP.DFlipFlop(mux_bus[13], clock, output_bus[13], not_14)
        FLOP.DFlipFlop(mux_bus[14], clock, output_bus[14], not_15)
        FLOP.DFlipFlop(mux_bus[15], clock, output_bus[15], not_16)

        self.data_bus = data_bus
        self.enable = enable
        self.clock = clock
        self.output_bus = output_bus

    def __str__(self):
        str_ = ""
        str_ += "data_bus: " + self.data_bus.__str__() + "\n"
        str_ += "enable: " + str(self.enable.value) + "\n"
        str_ += "clock: " + str(self.clock.value) + "\n"
        str_ += "output_bus: " + self.output_bus.__str__()
        return str_

    def __call__(
        self, *,
        data_bus=None,
        enable=None,
        clock=None,
        output_bus=None
    ):
        if data_bus is not None:
            self.data_bus.wire_values = data_bus
        if enable is not None:
            self.enable.value = enable
        if clock is not None:
            self.clock.value = clock
        if output_bus is not None:
            self.output_bus.wire_values = output_bus


class _Multiplexer2To1_4:
    """
    This is an internal module for Register4. It multiplexes two 4-bit inputs
    to a single 4-bit output.
    """
    def __init__(
        self,
        select,
        input_1_bus,
        input_2_bus,
        output_bus
    ):
        vcc = Wire()
        vcc.value = 1

        signal.Multiplexer2To1(
            vcc,
            select,
            input_1_bus[0],
            input_2_bus[0],
            output_bus[0]
        )
        signal.Multiplexer2To1(
            vcc,
            select,
            input_1_bus[1],
            input_2_bus[1],
            output_bus[1]
        )
        signal.Multiplexer2To1(
            vcc,
            select,
            input_1_bus[2],
            input_2_bus[2],
            output_bus[2]
        )
        signal.Multiplexer2To1(
            vcc,
            select,
            input_1_bus[3],
            input_2_bus[3],
            output_bus[3]
        )


class _Multiplexer2To1_8:
    """
    This is an internal module for Register8. It multiplexes two 8-bit inputs
    to a single 8-bit output.
    """
    def __init__(
        self,
        select,
        input_1_bus,
        input_2_bus,
        output_bus
    ):
        vcc = Wire()
        vcc.value = 1

        signal.Multiplexer2To1(
            vcc,
            select,
            input_1_bus[0],
            input_2_bus[0],
            output_bus[0]
        )
        signal.Multiplexer2To1(
            vcc,
            select,
            input_1_bus[1],
            input_2_bus[1],
            output_bus[1]
        )
        signal.Multiplexer2To1(
            vcc,
            select,
            input_1_bus[2],
            input_2_bus[2],
            output_bus[2]
        )
        signal.Multiplexer2To1(
            vcc,
            select,
            input_1_bus[3],
            input_2_bus[3],
            output_bus[3]
        )
        signal.Multiplexer2To1(
            vcc,
            select,
            input_1_bus[4],
            input_2_bus[4],
            output_bus[4]
        )
        signal.Multiplexer2To1(
            vcc,
            select,
            input_1_bus[5],
            input_2_bus[5],
            output_bus[5]
        )
        signal.Multiplexer2To1(
            vcc,
            select,
            input_1_bus[6],
            input_2_bus[6],
            output_bus[6]
        )
        signal.Multiplexer2To1(
            vcc,
            select,
            input_1_bus[7],
            input_2_bus[7],
            output_bus[7]
        )


class _Multiplexer2To1_16:
    """
    This is an internal module for Register16. It multiplexes two 16-bit inputs
    to a single 16-bit output.
    """
    def __init__(
        self,
        select,
        input_1_bus,
        input_2_bus,
        output_bus
    ):
        vcc = Wire()
        vcc.value = 1

        signal.Multiplexer2To1(
            vcc,
            select,
            input_1_bus[0],
            input_2_bus[0],
            output_bus[0]
        )
        signal.Multiplexer2To1(
            vcc,
            select,
            input_1_bus[1],
            input_2_bus[1],
            output_bus[1]
        )
        signal.Multiplexer2To1(
            vcc,
            select,
            input_1_bus[2],
            input_2_bus[2],
            output_bus[2]
        )
        signal.Multiplexer2To1(
            vcc,
            select,
            input_1_bus[3],
            input_2_bus[3],
            output_bus[3]
        )
        signal.Multiplexer2To1(
            vcc,
            select,
            input_1_bus[4],
            input_2_bus[4],
            output_bus[4]
        )
        signal.Multiplexer2To1(
            vcc,
            select,
            input_1_bus[5],
            input_2_bus[5],
            output_bus[5]
        )
        signal.Multiplexer2To1(
            vcc,
            select,
            input_1_bus[6],
            input_2_bus[6],
            output_bus[6]
        )
        signal.Multiplexer2To1(
            vcc,
            select,
            input_1_bus[7],
            input_2_bus[7],
            output_bus[7]
        )

        signal.Multiplexer2To1(
            vcc,
            select,
            input_1_bus[8],
            input_2_bus[8],
            output_bus[8]
        )
        signal.Multiplexer2To1(
            vcc,
            select,
            input_1_bus[9],
            input_2_bus[9],
            output_bus[9]
        )
        signal.Multiplexer2To1(
            vcc,
            select,
            input_1_bus[10],
            input_2_bus[10],
            output_bus[10]
        )
        signal.Multiplexer2To1(
            vcc,
            select,
            input_1_bus[11],
            input_2_bus[11],
            output_bus[11]
        )
        signal.Multiplexer2To1(
            vcc,
            select,
            input_1_bus[12],
            input_2_bus[12],
            output_bus[12]
        )
        signal.Multiplexer2To1(
            vcc,
            select,
            input_1_bus[13],
            input_2_bus[13],
            output_bus[13]
        )
        signal.Multiplexer2To1(
            vcc,
            select,
            input_1_bus[14],
            input_2_bus[14],
            output_bus[14]
        )
        signal.Multiplexer2To1(
            vcc,
            select,
            input_1_bus[15],
            input_2_bus[15],
            output_bus[15]
        )
