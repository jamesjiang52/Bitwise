"""
This module defines classes that simulate parity generators and parity
checkers. These two circuits have essentially the same structure, but perform
different functions. A parity generator transmits a single output, called a
parity bit, based on its inputs. A parity checker transmits a single output
that denotes an error if it has value 1.

All the parity generators in this module are even. If the input carries an even
number of 1's, the parity bit is 0. If the input carries an odd number of 1's,
the parity bit is 1. This ensures that the total number of 1's in the input and
the parity bit is even, which is the premise for a parity checker.

The parity checkers in this module are even. If the input carries an even
number of 1's, there is no error and the output is 0. If the input carries an
odd number of 1's, there has been an error in transmission and the output is 1.

The following classes are defined:
    ParityGenerator4
    ParityChecker4
    ParityGenerator8
    ParityChecker8
    ParityGenerator16
    ParityChecker16
"""

from .. import wire
from .. import gate

Wire = wire.Wire
Bus4 = wire.Bus4
Bus8 = wire.Bus8
Bus16 = wire.Bus16


class ParityGenerator4:
    """
    This parity generator has four inputs in a single 4-bit bus and a single
    output:
                     ________
        input_1 ----|        |---- parity_bit
        input_2 ----|        |
        input_3 ----|        |
        input_4 ----|________|

    If the number of 1's in the input is even, parity_bit is 0. If the number
    of 1's in the input is odd, parity_bit is 1.
    """
    def __init__(self, input_bus, parity_bit):
        if len(input_bus.wires) != 4:
            raise TypeError(
                "Expected bus of width 4, received bus of width {0}.".format(
                    len(input_bus.wires)
                )
            )

        wire_1 = Wire()
        wire_2 = Wire()

        gate.XORGate2(input_bus.wires[0], input_bus.wires[1], wire_1)
        gate.XORGate2(input_bus.wires[2], input_bus.wires[3], wire_2)
        gate.XORGate2(wire_1, wire_2, parity_bit)


class ParityChecker4:
    """
    This parity checker has four inputs in a single 4-bit bus, a parity_bit
    input, and a single output, denoting if an error has occurred:
                        ________
           input_1 ----|        |---- error
           input_2 ----|        |
           input_3 ----|        |
           input_4 ----|        |
        parity_bit ----|________|

    If the number of 1's in the input, including parity_bit, is even, no error
    has occurred and the output is 0. If the number of 1's in the input,
    including parity_bit, is odd, an error has occurred and the output is 1.
    """
    def __init__(self, input_bus, parity_bit, error):
        if len(input_bus.wires) != 4:
            raise TypeError(
                "Expected bus of width 4, received bus of width {0}.".format(
                    len(input_bus.wires)
                )
            )

        wire_1 = Wire()
        ParityGenerator4(input_bus, wire_1)
        gate.XORGate2(wire_1, parity_bit, error)


class ParityGenerator8:
    """
    This parity generator has eight inputs in a single 8-bit bus and a single
    output:
                     ________
        input_1 ----|        |---- parity_bit
        input_2 ----|        |
        input_3 ----|        |
        input_4 ----|        |
        input_5 ----|        |
        input_6 ----|        |
        input_7 ----|        |
        input_8 ----|________|

    If the number of 1's in the input is even, parity_bit is 0. If the number
    of 1's in the input is odd, parity_bit is 1.
    """
    def __init__(self, input_bus, parity_bit):
        if len(input_bus.wires) != 8:
            raise TypeError(
                "Expected bus of width 8, received bus of width {0}.".format(
                    len(input_bus.wires)
                )
            )

        wire_1 = Wire()
        wire_2 = Wire()
        bus_1 = Bus4(*input_bus.wires[0:4])
        bus_2 = Bus4(*input_bus.wires[4:8])

        ParityGenerator4(bus_1, wire_1)
        ParityGenerator4(bus_2, wire_2)
        gate.XORGate2(wire_1, wire_2, parity_bit)


class ParityChecker8:
    """
    This parity checker has eight inputs in a single 8-bit bus, a parity_bit
    input, and a single output, denoting if an error has occurred:
                        ________
           input_1 ----|        |---- error
           input_2 ----|        |
           input_3 ----|        |
           input_4 ----|        |
           input_5 ----|        |
           input_6 ----|        |
           input_7 ----|        |
           input_8 ----|        |
        parity_bit ----|________|

    If the number of 1's in the input, including parity_bit, is even, no error
    has occurred and the output is 0. If the number of 1's in the input,
    including parity_bit, is odd, an error has occurred and the output is 1.
    """
    def __init__(self, input_bus, parity_bit, error):
        if len(input_bus.wires) != 8:
            raise TypeError(
                "Expected bus of width 8, received bus of width {0}.".format(
                    len(input_bus.wires)
                )
            )

        wire_1 = Wire()
        ParityGenerator8(input_bus, wire_1)
        gate.XORGate2(wire_1, parity_bit, error)


class ParityGenerator16:
    """
    This parity generator has sixteen inputs in a single 16-bit bus and a
    single output:
                      ________
         input_1 ----|        |---- parity_bit
         input_2 ----|        |
         input_3 ----|        |
         input_4 ----|        |
         input_5 ----|        |
         input_6 ----|        |
         input_7 ----|        |
         input_8 ----|        |
         input_9 ----|        |
        input_10 ----|        |
        input_11 ----|        |
        input_12 ----|        |
        input_13 ----|        |
        input_14 ----|        |
        input_15 ----|        |
        input_16 ----|________|

    If the number of 1's in the input is even, parity_bit is 0. If the number
    of 1's in the input is odd, parity_bit is 1.
    """
    def __init__(self, input_bus, parity_bit):
        if len(input_bus.wires) != 16:
            raise TypeError(
                "Expected bus of width 16, received bus of width {0}.".format(
                    len(input_bus.wires)
                )
            )

        wire_1 = Wire()
        wire_2 = Wire()
        bus_1 = Bus8(*input_bus.wires[0:8])
        bus_2 = Bus8(*input_bus.wires[8:16])

        ParityGenerator8(bus_1, wire_1)
        ParityGenerator8(bus_2, wire_2)
        gate.XORGate2(wire_1, wire_2, parity_bit)


class ParityChecker16:
    """
    This parity checker has sixteen inputs in a single 16-bit bus, a parity_bit
    input, and a single output, denoting if an error has occurred:
                        ________
           input_1 ----|        |---- error
           input_2 ----|        |
           input_3 ----|        |
           input_4 ----|        |
           input_5 ----|        |
           input_6 ----|        |
           input_7 ----|        |
           input_8 ----|        |
           input_9 ----|        |
          input_10 ----|        |
          input_11 ----|        |
          input_12 ----|        |
          input_13 ----|        |
          input_14 ----|        |
          input_15 ----|        |
          input_16 ----|        |
        parity_bit ----|________|

    If the number of 1's in the input, including parity_bit, is even, no error
    has occurred and the output is 0. If the number of 1's in the input,
    including parity_bit, is odd, an error has occurred and the output is 1.
    """
    def __init__(self, input_bus, parity_bit, error):
        if len(input_bus.wires) != 16:
            raise TypeError(
                "Expected bus of width 16, received bus of width {0}.".format(
                    len(input_bus.wires)
                )
            )

        wire_1 = Wire()
        ParityGenerator16(input_bus, wire_1)
        gate.XORGate2(wire_1, parity_bit, error)
