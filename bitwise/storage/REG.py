"""
The following classes are defined:
    Register4
    Register8
    Register16
"""

from .. import wire
from . import FLOP

Wire = wire.Wire


class Register4:
    """Construct a new 4-bit storage register.

    Args:
        data_bus: An object of type Bus4. The data input to the register.
        clock: An object of type Wire or Clock. The clock input to the
            register.
        output_bus: An object of type Bus4. The output of the register. Takes
            on the value of data_bus on the positive edges of clock.

    Raises:
        TypeError: If either data_bus or output_bus is not a bus of width 4.
    """
    def __init__(self, input_bus, clock, output_bus):
        if len(input_bus) != 4:
            raise TypeError(
                "Expected bus of width 4, received bus of width {0}.".format(
                    len(input_bus)
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

        FLOP.DFlipFlop(input_bus[0], clock, output_bus[0], not_1)
        FLOP.DFlipFlop(input_bus[1], clock, output_bus[1], not_2)
        FLOP.DFlipFlop(input_bus[2], clock, output_bus[2], not_3)
        FLOP.DFlipFlop(input_bus[3], clock, output_bus[3], not_4)


class Register8:
    """Construct a new 8-bit storage register.

    Args:
        data_bus: An object of type Bus8. The data input to the register.
        clock: An object of type Wire or Clock. The clock input to the
            register.
        output_bus: An object of type Bus8. The output of the register. Takes
            on the value of data_bus on the positive edges of clock.

    Raises:
        TypeError: If either data_bus or output_bus is not a bus of width 8.
    """
    def __init__(self, input_bus, clock, output_bus):
        if len(input_bus) != 8:
            raise TypeError(
                "Expected bus of width 8, received bus of width {0}.".format(
                    len(input_bus)
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

        FLOP.DFlipFlop(input_bus[0], clock, output_bus[0], not_1)
        FLOP.DFlipFlop(input_bus[1], clock, output_bus[1], not_2)
        FLOP.DFlipFlop(input_bus[2], clock, output_bus[2], not_3)
        FLOP.DFlipFlop(input_bus[3], clock, output_bus[3], not_4)
        FLOP.DFlipFlop(input_bus[4], clock, output_bus[4], not_5)
        FLOP.DFlipFlop(input_bus[5], clock, output_bus[5], not_6)
        FLOP.DFlipFlop(input_bus[6], clock, output_bus[6], not_7)
        FLOP.DFlipFlop(input_bus[7], clock, output_bus[7], not_8)


class Register16:
    """Construct a new 16-bit storage register.

    Args:
        data_bus: An object of type Bus16. The data input to the register.
        clock: An object of type Wire or Clock. The clock input to the
            register.
        output_bus: An object of type Bus16. The output of the register. Takes
            on the value of data_bus on the positive edges of clock.

    Raises:
        TypeError: If either data_bus or output_bus is not a bus of width 16.
    """
    def __init__(self, input_bus, clock, output_bus):
        if len(input_bus) != 16:
            raise TypeError(
                "Expected bus of width 16, received bus of width {0}.".format(
                    len(input_bus)
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

        FLOP.DFlipFlop(input_bus[0], clock, output_bus[0], not_1)
        FLOP.DFlipFlop(input_bus[1], clock, output_bus[1], not_2)
        FLOP.DFlipFlop(input_bus[2], clock, output_bus[2], not_3)
        FLOP.DFlipFlop(input_bus[3], clock, output_bus[3], not_4)
        FLOP.DFlipFlop(input_bus[4], clock, output_bus[4], not_5)
        FLOP.DFlipFlop(input_bus[5], clock, output_bus[5], not_6)
        FLOP.DFlipFlop(input_bus[6], clock, output_bus[6], not_7)
        FLOP.DFlipFlop(input_bus[7], clock, output_bus[7], not_8)
        FLOP.DFlipFlop(input_bus[8], clock, output_bus[8], not_9)
        FLOP.DFlipFlop(input_bus[9], clock, output_bus[9], not_10)
        FLOP.DFlipFlop(input_bus[10], clock, output_bus[10], not_11)
        FLOP.DFlipFlop(input_bus[11], clock, output_bus[11], not_12)
        FLOP.DFlipFlop(input_bus[12], clock, output_bus[12], not_13)
        FLOP.DFlipFlop(input_bus[13], clock, output_bus[13], not_14)
        FLOP.DFlipFlop(input_bus[14], clock, output_bus[14], not_15)
        FLOP.DFlipFlop(input_bus[15], clock, output_bus[15], not_16)
