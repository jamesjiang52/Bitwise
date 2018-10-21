"""
The following classes are defined:
    SerialToParallelConverter1To4
    SerialToParallelConverter1To8
    SerialToParallelConverter1To16
"""

from .. import wire
from .. import storage

Wire = wire.Wire
Bus4 = wire.Bus4
Bus8 = wire.Bus8
Bus16 = wire.Bus16


class SerialToParallelConverter1To4:
    """

    """
    def __init__(self, enable, reset_n, data, clock, output_bus):
        if len(output_bus.wires) != 4:
            raise TypeError(
                "Expected bus of width 4, received bus of width {0}.".format(
                    len(output_bus.wires)
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

        storage.ShiftRegister4(
            enable,
            reset_n,
            vcc,
            d_bus,
            data,
            clock,
            output_bus,
            b
        )


class SerialToParallelConverter1To8:
    """

    """
    def __init__(self, enable, reset_n, data, clock, output_bus):
        if len(output_bus.wires) != 8:
            raise TypeError(
                "Expected bus of width 8, received bus of width {0}.".format(
                    len(output_bus.wires)
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

        storage.ShiftRegister8(
            enable,
            reset_n,
            vcc,
            d_bus,
            data,
            clock,
            output_bus,
            b
        )


class SerialToParallelConverter1To16:
    """

    """
    def __init__(self, enable, reset_n, data, clock, output_bus):
        if len(output_bus.wires) != 16:
            raise TypeError(
                "Expected bus of width 16, received bus of width {0}.".format(
                    len(output_bus.wires)
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

        storage.ShiftRegister16(
            enable,
            reset_n,
            vcc,
            d_bus,
            data,
            clock,
            output_bus,
            b
        )
