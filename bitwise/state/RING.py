"""
The following classes are defined:
    RingCounter4
    RingCounter8
    RingCounter16
"""

from .. import wire
from .. import signal
from . import COUNT

Wire = wire.Wire
Bus4 = wire.Bus4
Bus8 = wire.Bus8
Bus16 = wire.Bus16


class RingCounter4:
    """Construct a new 4-bit ring counter.

    Args:
        enable: An object of type Wire. Enables the ring counter.
        clear_n: An object of type Wire. Clears output_bus to (0, 0, 0, 1) (the
            0 state) asynchronously if its value is 0.
        clock: An object of type Wire or Clock. The clock input.
        output_bus: An object of type Bus4. The one-hot output of the ring
            counter. Starts at (0, 0, 0, 1) and counts up to (1, 0, 0, 0).

    Raises:
        TypeError: If output_bus is not a bus of width 4.
    """
    def __init__(self, enable, clear_n, clock, output_bus):
        if len(output_bus) != 4:
            raise TypeError(
                "Expected bus of width 4, received bus of width {0}.".format(
                    len(output_bus)
                )
            )

        vcc = Wire()
        vcc.value = 1
        output_1 = Wire()
        output_2 = Wire()

        COUNT.UpCounterMod4(enable, clear_n, clock, output_1, output_2)
        signal.Decoder1Of4(vcc, output_1, output_2, output_bus)

        self.enable = enable
        self.clear_n = clear_n
        self.clock = clock
        self.output_bus = output_bus

    def __str__(self):
        str_ = ""
        str_ += "enable: " + str(self.enable.value) + "\n"
        str_ += "clear_n: " + str(self.clear_n.value) + "\n"
        str_ += "clock: " + str(self.clock.value) + "\n"
        str_ += "output_bus: " + self.output_bus.__str__()
        return str_

    def __call__(
        self, *,
        enable=None,
        clear_n=None,
        clock=None,
        output_bus=None
    ):
        if enable is not None:
            self.enable.value = enable
        if clear_n is not None:
            self.clear_n.value = clear_n
        if clock is not None:
            self.clock.value = clock
        if output_bus is not None:
            self.output_bus.wire_values = output_bus


class RingCounter8:
    """Construct a new 8-bit ring counter.

    Args:
        enable: An object of type Wire. Enables the ring counter.
        clear_n: An object of type Wire. Clears output_bus to
            (0, 0, 0, 0, 0, 0, 0, 1) (the 0 state) asynchronously if its value
            is 0.
        clock: An object of type Wire or Clock. The clock input.
        output_bus: An object of type Bus8. The one-hot output of the ring
            counter. Starts at (0, 0, 0, 0, 0, 0, 0, 1) and counts up to
            (1, 0, 0, 0, 0, 0, 0, 0).

    Raises:
        TypeError: If output_bus is not a bus of width 8.
    """
    def __init__(self, enable, clear_n, clock, output_bus):
        if len(output_bus) != 8:
            raise TypeError(
                "Expected bus of width 8, received bus of width {0}.".format(
                    len(output_bus)
                )
            )

        vcc = Wire()
        vcc.value = 1
        output_1 = Wire()
        output_2 = Wire()
        output_3 = Wire()

        COUNT.UpCounterMod8(
            enable,
            clear_n,
            clock,
            output_1,
            output_2,
            output_3
        )
        signal.Decoder1Of8(vcc, output_1, output_2, output_3, output_bus)

        self.enable = enable
        self.clear_n = clear_n
        self.clock = clock
        self.output_bus = output_bus

    def __str__(self):
        str_ = ""
        str_ += "enable: " + str(self.enable.value) + "\n"
        str_ += "clear_n: " + str(self.clear_n.value) + "\n"
        str_ += "clock: " + str(self.clock.value) + "\n"
        str_ += "output_bus: " + self.output_bus.__str__()
        return str_

    def __call__(
        self, *,
        enable=None,
        clear_n=None,
        clock=None,
        output_bus=None
    ):
        if enable is not None:
            self.enable.value = enable
        if clear_n is not None:
            self.clear_n.value = clear_n
        if clock is not None:
            self.clock.value = clock
        if output_bus is not None:
            self.output_bus.wire_values = output_bus


class RingCounter16:
    """Construct a new 16-bit ring counter.

    Args:
        enable: An object of type Wire. Enables the ring counter.
        clear_n: An object of type Wire. Clears output_bus to
            (0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 1) (the 0 state)
            asynchronously if its value is 0.
        clock: An object of type Wire or Clock. The clock input.
        output_bus: An object of type Bus16. The one-hot output of the ring
            counter. Starts at (0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 1)
            and counts up to (1, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0).

    Raises:
        TypeError: If output_bus is not a bus of width 16.
    """
    def __init__(self, enable, clear_n, clock, output_bus):
        if len(output_bus) != 16:
            raise TypeError(
                "Expected bus of width 16, received bus of width {0}.".format(
                    len(output_bus)
                )
            )

        vcc = Wire()
        vcc.value = 1
        output_1 = Wire()
        output_2 = Wire()
        output_3 = Wire()
        output_4 = Wire()
        output_bus_int = Bus4(output_1, output_2, output_3, output_4)

        COUNT.UpCounterMod16(
            enable,
            clear_n,
            clock,
            output_bus_int
        )
        signal.Decoder1Of16(vcc, output_bus_int, output_bus)

        self.enable = enable
        self.clear_n = clear_n
        self.clock = clock
        self.output_bus = output_bus

    def __str__(self):
        str_ = ""
        str_ += "enable: " + str(self.enable.value) + "\n"
        str_ += "clear_n: " + str(self.clear_n.value) + "\n"
        str_ += "clock: " + str(self.clock.value) + "\n"
        str_ += "output_bus: " + self.output_bus.__str__()
        return str_

    def __call__(
        self, *,
        enable=None,
        clear_n=None,
        clock=None,
        output_bus=None
    ):
        if enable is not None:
            self.enable.value = enable
        if clear_n is not None:
            self.clear_n.value = clear_n
        if clock is not None:
            self.clock.value = clock
        if output_bus is not None:
            self.output_bus.wire_values = output_bus
