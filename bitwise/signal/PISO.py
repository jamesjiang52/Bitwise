"""
The following classes are defined:
    ParallelToSerialConverter4To1
    ParallelToSerialConverter8To1
    ParallelToSerialConverter16To1
"""

from .. import wire
from .. import storage

Wire = wire.Wire
Bus4 = wire.Bus4
Bus8 = wire.Bus8
Bus16 = wire.Bus16


class ParallelToSerialConverter4To1:
    """

    """
    def __init__(
        self,
        enable,
        reset_n,
        parallel_load_n,
        data_bus,
        clock,
        output
    ):
        if len(data_bus.wires) != 4:
            raise TypeError(
                "Expected bus of width 4, received bus of width {0}.".format(
                    len(data_bus.wires)
                )
            )

        vcc = Wire()
        vcc.value = 1
        gnd = Wire()
        gnd.value = 0

        d = Wire()
        b_1 = Wire()
        b_2 = Wire()
        b_3 = Wire()
        b_4 = Wire()
        b_bus = Bus4(b_1, b_2, b_3, b_4)

        storage.ShiftRegister4(
            enable,
            reset_n,
            parallel_load_n,
            data_bus,
            d,
            clock,
            b_bus,
            output
        )


class ParallelToSerialConverter8To1:
    """

    """
    def __init__(
        self,
        enable,
        reset_n,
        parallel_load_n,
        data_bus,
        clock,
        output
    ):
        if len(data_bus.wires) != 8:
            raise TypeError(
                "Expected bus of width 8, received bus of width {0}.".format(
                    len(data_bus.wires)
                )
            )

        vcc = Wire()
        vcc.value = 1
        gnd = Wire()
        gnd.value = 0

        d = Wire()
        b_1 = Wire()
        b_2 = Wire()
        b_3 = Wire()
        b_4 = Wire()
        b_5 = Wire()
        b_6 = Wire()
        b_7 = Wire()
        b_8 = Wire()
        b_bus = Bus8(b_1, b_2, b_3, b_4, b_5, b_6, b_7, b_8)

        storage.ShiftRegister8(
            enable,
            reset_n,
            parallel_load_n,
            data_bus,
            d,
            clock,
            b_bus,
            output
        )


class ParallelToSerialConverter16To1:
    """

    """
    def __init__(
        self,
        enable,
        reset_n,
        parallel_load_n,
        data_bus,
        clock,
        output
    ):
        if len(data_bus.wires) != 16:
            raise TypeError(
                "Expected bus of width 16, received bus of width {0}.".format(
                    len(data_bus.wires)
                )
            )

        vcc = Wire()
        vcc.value = 1
        gnd = Wire()
        gnd.value = 0

        d = Wire()
        b_1 = Wire()
        b_2 = Wire()
        b_3 = Wire()
        b_4 = Wire()
        b_5 = Wire()
        b_6 = Wire()
        b_7 = Wire()
        b_8 = Wire()
        b_9 = Wire()
        b_10 = Wire()
        b_11 = Wire()
        b_12 = Wire()
        b_13 = Wire()
        b_14 = Wire()
        b_15 = Wire()
        b_16 = Wire()
        b_bus = Bus16(
            b_1, b_2, b_3, b_4, b_5, b_6, b_7, b_8,
            b_9, b_10, b_11, b_12, b_13, b_14, b_15, b_16
        )

        storage.ShiftRegister16(
            enable,
            reset_n,
            parallel_load_n,
            data_bus,
            d,
            clock,
            b_bus,
            output
        )
