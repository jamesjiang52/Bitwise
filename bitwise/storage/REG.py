"""
This module defines classes that simulate storage registers. These registers
are simply arrays of single-bit flip-flops, and they serve the same purpose as
flip-flops as well, being used for storing data.

The following classes are defined:
    Register4
    Register8
    Register16
"""

from .. import wire
from . import FLOP

Wire = wire.Wire


class Register4:
    """
    This register has four inputs in a single 4-bit bus, a clock input, and
    four outputs in a single 4-bit bus:
                     ________
        input_1 ----|        |---- output_1
        input_2 ----|        |---- output_2
        input_3 ----|        |---- output_3
        input_4 ----|        |---- output_4
          clock ----|________|

    On the positive edge of clock (i.e. on the clock transition from a 0 value
    to a 1 value), the outputs takes on the value of the inputs, with output_1
    corresponding to input_1, output_2, corresponding to input_2, and so on.
    Otherwise, the outputs hold their current values.
    """
    def __init__(self, input_bus, clock, output_bus):
        if len(input_bus.wires) != 4:
            raise TypeError(
                "Expected bus of width 4, received bus of width {0}.".format(
                    len(input_bus.wires)
                )
            )

        if len(output_bus.wires) != 4:
            raise TypeError(
                "Expected bus of width 4, received bus of width {0}.".format(
                    len(output_bus.wires)
                )
            )

        not_1 = Wire()
        not_2 = Wire()
        not_3 = Wire()
        not_4 = Wire()

        FLOP.DFlipFlop(input_bus.wires[0], clock, output_bus.wires[0], not_1)
        FLOP.DFlipFlop(input_bus.wires[1], clock, output_bus.wires[1], not_2)
        FLOP.DFlipFlop(input_bus.wires[2], clock, output_bus.wires[2], not_3)
        FLOP.DFlipFlop(input_bus.wires[3], clock, output_bus.wires[3], not_4)


class Register8:
    """
    This register has eight inputs in a single 8-bit bus, a clock input, and
    eight outputs in a single 8-bit bus:
                     ________
        input_1 ----|        |---- output_1
        input_2 ----|        |---- output_2
        input_3 ----|        |---- output_3
        input_4 ----|        |---- output_4
        input_5 ----|        |---- output_5
        input_6 ----|        |---- output_6
        input_7 ----|        |---- output_7
        input_8 ----|        |---- output_8
          clock ----|________|

    On the positive edge of clock (i.e. on the clock transition from a 0 value
    to a 1 value), the outputs takes on the value of the inputs, with output_1
    corresponding to input_1, output_2, corresponding to input_2, and so on.
    Otherwise, the outputs hold their current values.
    """
    def __init__(self, input_bus, clock, output_bus):
        if len(input_bus.wires) != 8:
            raise TypeError(
                "Expected bus of width 8, received bus of width {0}.".format(
                    len(input_bus.wires)
                )
            )

        if len(output_bus.wires) != 8:
            raise TypeError(
                "Expected bus of width 8, received bus of width {0}.".format(
                    len(output_bus.wires)
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

        FLOP.DFlipFlop(input_bus.wires[0], clock, output_bus.wires[0], not_1)
        FLOP.DFlipFlop(input_bus.wires[1], clock, output_bus.wires[1], not_2)
        FLOP.DFlipFlop(input_bus.wires[2], clock, output_bus.wires[2], not_3)
        FLOP.DFlipFlop(input_bus.wires[3], clock, output_bus.wires[3], not_4)
        FLOP.DFlipFlop(input_bus.wires[4], clock, output_bus.wires[4], not_5)
        FLOP.DFlipFlop(input_bus.wires[5], clock, output_bus.wires[5], not_6)
        FLOP.DFlipFlop(input_bus.wires[6], clock, output_bus.wires[6], not_7)
        FLOP.DFlipFlop(input_bus.wires[7], clock, output_bus.wires[7], not_8)


class Register16:
    """
    This register has sixteen inputs in a single 16-bit bus, a clock input, and
    sixteen outputs in a single 16-bit bus:
                      ________
         input_1 ----|        |---- output_1
         input_2 ----|        |---- output_2
         input_3 ----|        |---- output_3
         input_4 ----|        |---- output_4
         input_5 ----|        |---- output_5
         input_6 ----|        |---- output_6
         input_7 ----|        |---- output_7
         input_8 ----|        |---- output_8
         input_9 ----|        |---- output_9
        input_10 ----|        |---- output_10
        input_11 ----|        |---- output_11
        input_12 ----|        |---- output_12
        input_13 ----|        |---- output_13
        input_14 ----|        |---- output_14
        input_15 ----|        |---- output_15
        input_16 ----|        |---- output_16
           clock ----|________|

    On the positive edge of clock (i.e. on the clock transition from a 0 value
    to a 1 value), the outputs takes on the value of the inputs, with output_1
    corresponding to input_1, output_2, corresponding to input_2, and so on.
    Otherwise, the outputs hold their current values.
    """
    def __init__(self, input_bus, clock, output_bus):
        if len(input_bus.wires) != 16:
            raise TypeError(
                "Expected bus of width 16, received bus of width {0}.".format(
                    len(input_bus.wires)
                )
            )

        if len(output_bus.wires) != 16:
            raise TypeError(
                "Expected bus of width 16, received bus of width {0}.".format(
                    len(output_bus.wires)
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

        output = output_bus.wires

        FLOP.DFlipFlop(input_bus.wires[0], clock, output[0], not_1)
        FLOP.DFlipFlop(input_bus.wires[1], clock, output[1], not_2)
        FLOP.DFlipFlop(input_bus.wires[2], clock, output[2], not_3)
        FLOP.DFlipFlop(input_bus.wires[3], clock, output[3], not_4)
        FLOP.DFlipFlop(input_bus.wires[4], clock, output[4], not_5)
        FLOP.DFlipFlop(input_bus.wires[5], clock, output[5], not_6)
        FLOP.DFlipFlop(input_bus.wires[6], clock, output[6], not_7)
        FLOP.DFlipFlop(input_bus.wires[7], clock, output[7], not_8)
        FLOP.DFlipFlop(input_bus.wires[8], clock, output[8], not_9)
        FLOP.DFlipFlop(input_bus.wires[9], clock, output[9], not_10)
        FLOP.DFlipFlop(input_bus.wires[10], clock, output[10], not_11)
        FLOP.DFlipFlop(input_bus.wires[11], clock, output[11], not_12)
        FLOP.DFlipFlop(input_bus.wires[12], clock, output[12], not_13)
        FLOP.DFlipFlop(input_bus.wires[13], clock, output[13], not_14)
        FLOP.DFlipFlop(input_bus.wires[14], clock, output[14], not_15)
        FLOP.DFlipFlop(input_bus.wires[15], clock, output[15], not_16)
