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
    """Construct a new 4-bit-parallel-to-serial converter.

    Args:
        enable: An object of type Wire. Enables the converter.
        clear_n: An object of type Wire. Clears all 4 internal registers to 0
            asynchronously if its value is 0.
        load_n: An object of type Wire. The mode select. A value of 0 indicates
            a parallel load operation, where the values of data_bus are loaded
            into the internal registers. A value of 1 indicates a shift-right
            operation.
        data_bus: An object of type Bus4. The parallel data input.
        clock: An object of type Wire or Clock. The clock input.
        output: An object of type Wire. The serial output of the converter.
            data_bus[3] is outputted first, and data_bus[0] is outputted last.

    Raises:
        TypeError: If data_bus is not a bus of width 4.
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
        if len(data_bus) != 4:
            raise TypeError(
                "Expected bus of width 4, received bus of width {0}.".format(
                    len(data_bus)
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
    """Construct a new 8-bit-parallel-to-serial converter.

    Args:
        enable: An object of type Wire. Enables the converter.
        clear_n: An object of type Wire. Clears all 8 internal registers to 0
            asynchronously if its value is 0.
        load_n: An object of type Wire. The mode select. A value of 0 indicates
            a parallel load operation, where the values of data_bus are loaded
            into the internal registers. A value of 1 indicates a shift-right
            operation.
        data_bus: An object of type Bus8. The parallel data input.
        clock: An object of type Wire or Clock. The clock input.
        output: An object of type Wire. The serial output of the converter.
            data_bus[7] is outputted first, and data_bus[0] is outputted last.

    Raises:
        TypeError: If data_bus is not a bus of width 8.
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
        if len(data_bus) != 8:
            raise TypeError(
                "Expected bus of width 8, received bus of width {0}.".format(
                    len(data_bus)
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
    """Construct a new 16-bit-parallel-to-serial converter.

    Args:
        enable: An object of type Wire. Enables the converter.
        clear_n: An object of type Wire. Clears all 16 internal registers to 0
            asynchronously if its value is 0.
        load_n: An object of type Wire. The mode select. A value of 0 indicates
            a parallel load operation, where the values of data_bus are loaded
            into the internal registers. A value of 1 indicates a shift-right
            operation.
        data_bus: An object of type Bus16. The parallel data input.
        clock: An object of type Wire or Clock. The clock input.
        output: An object of type Wire. The serial output of the converter.
            data_bus[15] is outputted first, and data_bus[0] is outputted last.

    Raises:
        TypeError: If data_bus is not a bus of width 16.
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
        if len(data_bus) != 16:
            raise TypeError(
                "Expected bus of width 16, received bus of width {0}.".format(
                    len(data_bus)
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
