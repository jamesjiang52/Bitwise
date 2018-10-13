"""
This module defines classes that simulate logical comparators. While there are
many different forms of comparators, the ones in this module simply receive two
signed binary numbers in two's complement form and determine if the first is
greater than, equal to, or less than the second.

The following classes are defined:
    Comparator3
    Comparator7
    Comparator15
"""

from .. import wire
from .. import gate
from .. import arithmetic

Wire = wire.Wire
Bus4 = wire.Bus4
Bus8 = wire.Bus8
Bus16 = wire.Bus16


class Comparator3:
    """
    This logical comparator has eight inputs in two 4-bit buses and three
    outputs:
                     ________
        input_1 ----|        |---- greater_than
        input_2 ----|        |---- equal_to
        input_3 ----|        |---- less_than
        input_4 ----|        |
        input_5 ----|        |
        input_6 ----|        |
        input_7 ----|        |
        input_8 ----|________|

    The comparator compares two signed 3-bit binary numbers. The first number
    has input_1, input_2, and input_4 as the sign bit, MSB, and LSB,
    respectively (input_bus_1). The second number has input_5, input_6, and
    input_8 as the sign bit, MSB, and LSB, respectively (input_bus_2). If the
    first number is greater than the second, the greater_than output will be 1,
    with all other outputs 0. The other outputs work analogously.
    """
    def __init__(self, input_bus_1, input_bus_2, gt, z, lt):
        if len(input_bus_1.wires) != 4:
            raise TypeError(
                "Expected bus of width 4, received bus of width {0}.".format(
                    len(input_bus_1.wires)
                )
            )

        if len(input_bus_2.wires) != 4:
            raise TypeError(
                "Expected bus of width 4, received bus of width {0}.".format(
                    len(input_bus_2.wires)
                )
            )

        add_subtract = Wire()
        add_subtract.value = 1
        overflow = Wire()
        carry_out = Wire()
        wire_1 = Wire()
        wire_2 = Wire()
        wire_3 = Wire()
        lt_or_z = Wire()
        N = Wire()
        adder_out = Bus4(N, wire_1, wire_2, wire_3)

        arithmetic.AdderSubtractor4(
            add_subtract,
            input_bus_1,
            input_bus_2,
            overflow,
            carry_out,
            adder_out
        )

        gate.NORGate4(*adder_out.wires, z)
        gate.XORGate2(N, overflow, lt)
        gate.ORGate2(z, lt, lt_or_z)
        gate.NOTGate(lt_or_z, gt)


class Comparator7:
    """
    This logical comparator has sixteen inputs in two 8-bit buses and three
    outputs:
                      ________
         input_1 ----|        |---- greater_than
         input_2 ----|        |---- equal_to
         input_3 ----|        |---- less_than
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

    The comparator compares two signed 7-bit binary numbers. The first number
    has input_1, input_2, and input_8 as the sign bit, MSB, and LSB,
    respectively (input_bus_1). The second number has input_9, input_10, and
    input_16 as the sign bit, MSB, and LSB, respectively (input_bus_2). If the
    first number is greater than the second, the greater_than output will be 1,
    with all other outputs 0. The other outputs work analogously.
    """
    def __init__(self, input_bus_1, input_bus_2, gt, z, lt):
        if len(input_bus_1.wires) != 8:
            raise TypeError(
                "Expected bus of width 8, received bus of width {0}.".format(
                    len(input_bus_1.wires)
                )
            )

        if len(input_bus_2.wires) != 8:
            raise TypeError(
                "Expected bus of width 8, received bus of width {0}.".format(
                    len(input_bus_2.wires)
                )
            )

        add_subtract = Wire()
        add_subtract.value = 1
        overflow = Wire()
        carry_out = Wire()
        wire_1 = Wire()
        wire_2 = Wire()
        wire_3 = Wire()
        wire_4 = Wire()
        wire_5 = Wire()
        wire_6 = Wire()
        wire_7 = Wire()
        lt_or_z = Wire()
        N = Wire()
        or_1 = Wire()
        or_2 = Wire()
        adder_out = Bus8(
            N,
            wire_1,
            wire_2,
            wire_3,
            wire_4,
            wire_5,
            wire_6,
            wire_7
        )

        arithmetic.AdderSubtractor8(
            add_subtract,
            input_bus_1,
            input_bus_2,
            overflow,
            carry_out,
            adder_out
        )

        gate.ORGate4(*adder_out.wires[0:4], or_1)
        gate.ORGate4(*adder_out.wires[4:8], or_2)
        gate.NORGate2(or_1, or_2, z)
        gate.XORGate2(N, overflow, lt)
        gate.ORGate2(z, lt, lt_or_z)
        gate.NOTGate(lt_or_z, gt)


class Comparator15:
    """
    This logical comparator has thirty-two inputs in two 16-bit buses and three
    outputs:
                      ________
         input_1 ----|        |---- greater_than
         input_2 ----|        |---- equal_to
         input_3 ----|        |---- less_than
         input_4 ----|        |
             ...     |        |
        input_13 ----|        |
        input_14 ----|        |
        input_15 ----|        |
        input_16 ----|        |
        input_17 ----|        |
        input_18 ----|        |
        input_19 ----|        |
        input_20 ----|        |
             ...     |        |
        input_29 ----|        |
        input_30 ----|        |
        input_31 ----|        |
        input_32 ----|________|

    The comparator compares two signed 15-bit binary numbers. The first number
    has input_1, input_2, and input_16 as the sign bit, MSB, and LSB,
    respectively (input_bus_1). The second number has input_17, input_18, and
    input_32 as the sign bit, MSB, and LSB, respectively (input_bus_2). If the
    first number is greater than the second, the greater_than output will be 1,
    with all other outputs 0. The other outputs work analogously.
    """
    def __init__(self, input_bus_1, input_bus_2, gt, z, lt):
        if len(input_bus_1.wires) != 16:
            raise TypeError(
                "Expected bus of width 16, received bus of width {0}.".format(
                    len(input_bus_1.wires)
                )
            )

        if len(input_bus_2.wires) != 16:
            raise TypeError(
                "Expected bus of width 16, received bus of width {0}.".format(
                    len(input_bus_2.wires)
                )
            )

        add_subtract = Wire()
        add_subtract.value = 1
        overflow = Wire()
        carry_out = Wire()
        wire_1 = Wire()
        wire_2 = Wire()
        wire_3 = Wire()
        wire_4 = Wire()
        wire_5 = Wire()
        wire_6 = Wire()
        wire_7 = Wire()
        wire_8 = Wire()
        wire_9 = Wire()
        wire_10 = Wire()
        wire_11 = Wire()
        wire_12 = Wire()
        wire_13 = Wire()
        wire_14 = Wire()
        wire_15 = Wire()
        lt_or_z = Wire()
        N = Wire()
        or_1 = Wire()
        or_2 = Wire()
        or_3 = Wire()
        or_4 = Wire()
        adder_out = Bus16(
            N,
            wire_1,
            wire_2,
            wire_3,
            wire_4,
            wire_5,
            wire_6,
            wire_7,
            wire_8,
            wire_9,
            wire_10,
            wire_11,
            wire_12,
            wire_13,
            wire_14,
            wire_15
        )

        arithmetic.AdderSubtractor16(
            add_subtract,
            input_bus_1,
            input_bus_2,
            overflow,
            carry_out,
            adder_out
        )

        gate.ORGate4(*adder_out.wires[0:4], or_1)
        gate.ORGate4(*adder_out.wires[4:8], or_2)
        gate.ORGate4(*adder_out.wires[8:12], or_3)
        gate.ORGate4(*adder_out.wires[12:16], or_4)
        gate.NORGate4(or_1, or_2, or_3, or_4, z)
        gate.XORGate2(N, overflow, lt)
        gate.ORGate2(z, lt, lt_or_z)
        gate.NOTGate(lt_or_z, gt)
