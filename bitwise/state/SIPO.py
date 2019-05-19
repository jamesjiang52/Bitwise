"""
The following classes are defined:
    SerialToParallelConverter1To4
    SerialToParallelConverter1To8
    SerialToParallelConverter1To16
"""

from .. import wire
from . import SHIFT

Wire = wire.Wire
Bus4 = wire.Bus4
Bus8 = wire.Bus8
Bus16 = wire.Bus16


class SerialToParallelConverter1To4:
    """Construct a new serial-to-4-bit-parallel converter.

    Args:
        enable: An object of type Wire. Enables the converter.
        clear_n: An object of type Wire. Clears all 4 internal registers to 0
            asynchronously if its value is 0.
        data: An object of type Wire. The serial data input.
        clock: An object of type Wire or Clock. The clock input.
        output_bus: An object of type Bus4. The parallel output of the
            converter. output[0] corresponds to the most recent serial data
            input.

    Raises:
        TypeError: If output_bus is not a bus of width 4.
    """
    def __init__(self, enable, clear_n, data, clock, output_bus):
        if len(output_bus) != 4:
            raise TypeError(
                "Expected bus of width 4, received bus of width {0}.".format(
                    len(output_bus)
                )
            )

        vcc = Wire()
        vcc.value = 1
        gnd = Wire()
        gnd.value = 0

        d_1 = Wire()
        d_2 = Wire()
        d_3 = Wire()
        d_4 = Wire()
        b = Wire()
        d_bus = Bus4(d_1, d_2, d_3, d_4)

        SHIFT.ShiftRegister4(
            enable,
            clear_n,
            vcc,
            d_bus,
            data,
            clock,
            output_bus,
            b
        )

        self.enable = enable
        self.clear_n = clear_n
        self.data = data
        self.clock = clock
        self.output_bus = output_bus

    def __str__(self):
        str_ = ""
        str_ += "enable: " + str(self.enable.value) + "\n"
        str_ += "clear_n: " + str(self.clear_n.value) + "\n"
        str_ += "data: " + str(self.data.value) + "\n"
        str_ += "clock: " + str(self.clock.value) + "\n"
        str_ += "output_bus: " + self.output_bus.__str__()
        return str_

    def __call__(
        self, *,
        enable=None,
        clear_n=None,
        data=None,
        clock=None,
        output_bus=None
    ):
        if enable is not None:
            self.enable.value = enable
        if clear_n is not None:
            self.clear_n.value = clear_n
        if data is not None:
            self.data.value = data
        if clock is not None:
            self.clock.value = clock
        if output_bus is not None:
            self.output_bus.wire_values = output_bus


class SerialToParallelConverter1To8:
    """Construct a new serial-to-8-bit-parallel converter.

    Args:
        enable: An object of type Wire. Enables the converter.
        clear_n: An object of type Wire. Clears all 8 internal registers to 0
            asynchronously if its value is 0.
        data: An object of type Wire. The serial data input.
        clock: An object of type Wire or Clock. The clock input.
        output_bus: An object of type Bus8. The parallel output of the
            converter. output[0] corresponds to the most recent serial data
            input.

    Raises:
        TypeError: If output_bus is not a bus of width 8.
    """
    def __init__(self, enable, clear_n, data, clock, output_bus):
        if len(output_bus) != 8:
            raise TypeError(
                "Expected bus of width 8, received bus of width {0}.".format(
                    len(output_bus)
                )
            )

        vcc = Wire()
        vcc.value = 1
        gnd = Wire()
        gnd.value = 0

        d_1 = Wire()
        d_2 = Wire()
        d_3 = Wire()
        d_4 = Wire()
        d_5 = Wire()
        d_6 = Wire()
        d_7 = Wire()
        d_8 = Wire()
        b = Wire()
        d_bus = Bus8(d_1, d_2, d_3, d_4, d_5, d_6, d_7, d_8)

        SHIFT.ShiftRegister8(
            enable,
            clear_n,
            vcc,
            d_bus,
            data,
            clock,
            output_bus,
            b
        )

        self.enable = enable
        self.clear_n = clear_n
        self.data = data
        self.clock = clock
        self.output_bus = output_bus

    def __str__(self):
        str_ = ""
        str_ += "enable: " + str(self.enable.value) + "\n"
        str_ += "clear_n: " + str(self.clear_n.value) + "\n"
        str_ += "data: " + str(self.data.value) + "\n"
        str_ += "clock: " + str(self.clock.value) + "\n"
        str_ += "output_bus: " + self.output_bus.__str__()
        return str_

    def __call__(
        self, *,
        enable=None,
        clear_n=None,
        data=None,
        clock=None,
        output_bus=None
    ):
        if enable is not None:
            self.enable.value = enable
        if clear_n is not None:
            self.clear_n.value = clear_n
        if data is not None:
            self.data.value = data
        if clock is not None:
            self.clock.value = clock
        if output_bus is not None:
            self.output_bus.wire_values = output_bus


class SerialToParallelConverter1To16:
    """Construct a new serial-to-16-bit-parallel converter.

    Args:
        enable: An object of type Wire. Enables the converter.
        clear_n: An object of type Wire. Clears all 16 internal registers to 0
            asynchronously if its value is 0.
        data: An object of type Wire. The serial data input.
        clock: An object of type Wire or Clock. The clock input.
        output_bus: An object of type Bus16. The parallel output of the
            converter. output[0] corresponds to the most recent serial data
            input.

    Raises:
        TypeError: If output_bus is not a bus of width 16.
    """
    def __init__(self, enable, clear_n, data, clock, output_bus):
        if len(output_bus) != 16:
            raise TypeError(
                "Expected bus of width 16, received bus of width {0}.".format(
                    len(output_bus)
                )
            )

        vcc = Wire()
        vcc.value = 1
        gnd = Wire()
        gnd.value = 0

        d_1 = Wire()
        d_2 = Wire()
        d_3 = Wire()
        d_4 = Wire()
        d_5 = Wire()
        d_6 = Wire()
        d_7 = Wire()
        d_8 = Wire()
        d_9 = Wire()
        d_10 = Wire()
        d_11 = Wire()
        d_12 = Wire()
        d_13 = Wire()
        d_14 = Wire()
        d_15 = Wire()
        d_16 = Wire()
        b = Wire()
        d_bus = Bus16(
            d_1, d_2, d_3, d_4, d_5, d_6, d_7, d_8,
            d_9, d_10, d_11, d_12, d_13, d_14, d_15, d_16
        )

        SHIFT.ShiftRegister16(
            enable,
            clear_n,
            vcc,
            d_bus,
            data,
            clock,
            output_bus,
            b
        )

        self.enable = enable
        self.clear_n = clear_n
        self.data = data
        self.clock = clock
        self.output_bus = output_bus

    def __str__(self):
        str_ = ""
        str_ += "enable: " + str(self.enable.value) + "\n"
        str_ += "clear_n: " + str(self.clear_n.value) + "\n"
        str_ += "data: " + str(self.data.value) + "\n"
        str_ += "clock: " + str(self.clock.value) + "\n"
        str_ += "output_bus: " + self.output_bus.__str__()
        return str_

    def __call__(
        self, *,
        enable=None,
        clear_n=None,
        data=None,
        clock=None,
        output_bus=None
    ):
        if enable is not None:
            self.enable.value = enable
        if clear_n is not None:
            self.clear_n.value = clear_n
        if data is not None:
            self.data.value = data
        if clock is not None:
            self.clock.value = clock
        if output_bus is not None:
            self.output_bus.wire_values = output_bus
