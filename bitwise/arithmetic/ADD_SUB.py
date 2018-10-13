"""
This modules defines classes that simulate adder/subtractor circuits. Unlike
the adders in ADD.py, these circuits are designed to work with both unsigned
and signed integers, and can perform both addition and subtraction operations.
This choice is made via an add_subtract select input.

The following classes are defined:
    AdderSubtractor4
    AdderSubtractor8
    AdderSubtractor16
"""

from .. import wire
from .. import gate
from .. import signal
from . import ADD

Wire = wire.Wire
Bus4 = wire.Bus4
Bus8 = wire.Bus8
Bus16 = wire.Bus16


class AdderSubtractor4:
    """
    This adder/subtractor has eight inputs in two 4-bit buses, an add_subtract
    select input, four outputs in a single 4-bit bus, a carry_out output, and
    an overflow output:
                          ________
        add_subtract ----|        |---- overflow
             input_1 ----|        |---- carry_out
             input_2 ----|        |---- output_1
             input_3 ----|        |---- output_2
             input_4 ----|        |---- output_3
             input_5 ----|        |---- output_4
             input_6 ----|        |
             input_7 ----|        |
             input_8 ----|________|

    If the add_subtract select is set to 0, this circuit computes the sum of
    two 4-bit numbers. Inputs input_1 and input_4 correspond to the MSB and
    LSB, respectively, of the first addend (input_bus_1), and inputs input_5
    and input_8 correspond to the MSB and LSB, respectively, of the second
    addend (input_bus_2). The outputs will then have carry_out and output_4 as
    the MSB and LSB, respectively. The overflow output for a 0 add_subtract
    select is meaningless.

    If the add_subtract select is set to 1, this circuit computes the
    difference between two signed 3-bit numbers in two's complement form.
    Inputs input_1 and input_5 correspond to the sign bit of the minuend
    (input_bus_1) and the subtrahend (input_bus_2), respectively. The minuend
    has input_2 as the MSB and input_4 as the LSB. The subtrahend has input_6
    as the MSB and input_8 as the LSB. The outputs will then have output_2 and
    output_4 as the MSB and LSB, respectively, with output_1 being the sign
    bit. The overflow denotes whether or not arithmetic overflow has occurred
    and the outputs should be discarded; it is 1 if overflow has occurred, and
    0 otherwise. The carry_out output for a 1 add_subtract select is
    meaningless.
    """
    def __init__(
            self,
            add_subtract,
            input_bus_1,
            input_bus_2,
            overflow,
            carry_out,
            output_bus
            ):
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

        if len(output_bus.wires) != 4:
            raise TypeError(
                "Expected bus of width 4, received bus of width {0}.".format(
                    len(output_bus.wires)
                )
            )

        wire_1 = Wire()
        wire_2 = Wire()
        wire_3 = Wire()
        wire_4 = Wire()
        not_input_1 = Wire()
        not_input_2 = Wire()
        not_output = Wire()
        and_1_wire = Wire()
        and_2_wire = Wire()
        bus_1 = Bus4(wire_1, wire_2, wire_3, wire_4)
        input_1 = input_bus_1.wires
        input_2 = input_bus_2.wires
        output = output_bus.wires

        signal.ControlledInverter4(add_subtract, input_bus_2, bus_1)
        ADD.Adder4(add_subtract, input_bus_1, bus_1, carry_out, output_bus)

        gate.NOTGate(input_1[0], not_input_1)
        gate.NOTGate(input_2[0], not_input_2)
        gate.NOTGate(output[0], not_output)
        gate.ANDGate3(input_1[0], not_input_2, not_output, and_1_wire)
        gate.ANDGate3(not_input_1, input_2[0], output[0], and_2_wire)
        gate.ORGate2(and_1_wire, and_2_wire, overflow)


class AdderSubtractor8:
    """
    This adder/subtractor has sixteen inputs in two 8-bit buses, an
    add_subtract select input, eight outputs in a single 8-bit bus, a carry_out
    output, and an overflow output:
                          ________
        add_subtract ----|        |---- overflow
             input_1 ----|        |---- carry_out
             input_2 ----|        |---- output_1
             input_3 ----|        |---- output_2
             input_4 ----|        |---- output_3
             input_5 ----|        |---- output_4
             input_6 ----|        |---- output_5
             input_7 ----|        |---- output_6
             input_8 ----|        |---- output_7
             input_9 ----|        |---- output_8
            input_10 ----|        |
            input_11 ----|        |
            input_12 ----|        |
            input_13 ----|        |
            input_14 ----|        |
            input_15 ----|        |
            input_16 ----|________|

    If the add_subtract select is set to 0, this circuit computes the sum of
    two 8-bit numbers. Inputs input_1 and input_8 correspond to the MSB and
    LSB, respectively, of the first addend (input_bus_1), and inputs input_9
    and input_16 correspond to the MSB and LSB, respectively, of the second
    addend (input_bus_2). The outputs will then have carry_out and output_8 as
    the MSB and LSB, respectively. The overflow output for a 0 add_subtract
    select is meaningless.

    If the add_subtract select is set to 1, this circuit computes the
    difference between two signed 7-bit numbers in two's complement form.
    Inputs input_1 and input_9 correspond to the sign bit of the minuend (
    input_bus_1) and the subtrahend (input_bus_2), respectively. The minuend
    has input_2 as the MSB and input_8 as the LSB. The subtrahend has input_10
    as the MSB and input_16 as the LSB. The outputs will then have output_2 and
    output_8 as the MSB and LSB, respectively, with output_1 being the sign
    bit. The overflow denotes whether or not arithmetic overflow has occurred
    and the outputs should be discarded; it is 1 if overflow has occurred, and
    0 otherwise. The carry_out output for a 1 add_subtract select is
    meaningless.
    """
    def __init__(
            self,
            add_subtract,
            input_bus_1,
            input_bus_2,
            overflow,
            carry_out,
            output_bus
            ):
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

        if len(output_bus.wires) != 8:
            raise TypeError(
                "Expected bus of width 8, received bus of width {0}.".format(
                    len(output_bus.wires)
                )
            )

        wire_1 = Wire()
        wire_2 = Wire()
        wire_3 = Wire()
        wire_4 = Wire()
        wire_5 = Wire()
        wire_6 = Wire()
        wire_7 = Wire()
        wire_8 = Wire()
        not_input_1 = Wire()
        not_input_2 = Wire()
        not_output = Wire()
        and_1_wire = Wire()
        and_2_wire = Wire()
        bus_1 = Bus8(
            wire_1,
            wire_2,
            wire_3,
            wire_4,
            wire_5,
            wire_6,
            wire_7,
            wire_8
        )
        input_1 = input_bus_1.wires
        input_2 = input_bus_2.wires
        output = output_bus.wires

        signal.ControlledInverter8(add_subtract, input_bus_2, bus_1)
        ADD.Adder8(add_subtract, input_bus_1, bus_1, carry_out, output_bus)

        gate.NOTGate(input_1[0], not_input_1)
        gate.NOTGate(input_2[0], not_input_2)
        gate.NOTGate(output[0], not_output)
        gate.ANDGate3(input_1[0], not_input_2, not_output, and_1_wire)
        gate.ANDGate3(not_input_1, input_2[0], output[0], and_2_wire)
        gate.ORGate2(and_1_wire, and_2_wire, overflow)


class AdderSubtractor16:
    """
    This adder/subtractor has thirty-two inputs in two 16-bit buses, an
    add_subtract select input, sixteen outputs in a single 16-bit bus, a
    carry_out output, and an overflow output:
                          ________
        add_subtract ----|        |---- overflow
             input_1 ----|        |---- carry_out
             input_2 ----|        |---- output_1
             input_3 ----|        |---- output_2
             input_4 ----|        |---- output_3
                 ...     |        |---- output_4
            input_13 ----|        |---- output_5
            input_14 ----|        |---- output_6
            input_15 ----|        |---- output_7
            input_16 ----|        |---- output_8
            input_17 ----|        |---- output_9
            input_18 ----|        |---- output_10
            input_19 ----|        |---- output_11
            input_20 ----|        |---- output_12
                 ...     |        |---- output_13
            input_29 ----|        |---- output_14
            input_30 ----|        |---- output_15
            input_31 ----|        |---- output_16
            input_32 ----|________|

    If the add_subtract select is set to 0, this circuit computes the sum of
    two 16-bit numbers. Inputs input_1 and input_16 correspond to the MSB and
    LSB, respectively, of the first addend (input_bus_1), and inputs input_17
    and input_32 correspond to the MSB and LSB, respectively, of the second
    addend (input_bus_2). The outputs will then have carry_out and output_16 as
    the MSB and LSB, respectively. The overflow output for a 0 add_subtract
    select is meaningless.

    If the add_subtract select is set to 1, this circuit computes the
    difference between two signed 15-bit numbers in two's complement form.
    Inputs input_1 and input_17 correspond to the sign bit of the minuend
    (input_bus_1) and the subtrahend (input_bus_2), respectively. The minuend
    has input_2 as the MSB and input_16 as the LSB. The subtrahend has input_18
    as the MSB and input_32 as the LSB. The outputs will then have output_2 and
    output_16 as the MSB and LSB, respectively, with output_1 being the sign
    bit. The overflow denotes whether or not arithmetic overflow has occurred
    and the outputs should be discarded; it is 1 if overflow has occurred, and
    0 otherwise. The carry_out output for a 1 add_subtract select is
    meaningless.
    """
    def __init__(
            self,
            add_subtract,
            input_bus_1,
            input_bus_2,
            overflow,
            carry_out,
            output_bus
            ):
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

        if len(output_bus.wires) != 16:
            raise TypeError(
                "Expected bus of width 16, received bus of width {0}.".format(
                    len(output_bus.wires)
                )
            )

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
        wire_16 = Wire()
        not_input_1 = Wire()
        not_input_2 = Wire()
        not_output = Wire()
        and_1_wire = Wire()
        and_2_wire = Wire()
        bus_1 = Bus16(
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
            wire_15,
            wire_16
        )
        input_1 = input_bus_1.wires
        input_2 = input_bus_2.wires
        output = output_bus.wires

        signal.ControlledInverter16(add_subtract, input_bus_2, bus_1)
        ADD.Adder16(add_subtract, input_bus_1, bus_1, carry_out, output_bus)

        gate.NOTGate(input_1[0], not_input_1)
        gate.NOTGate(input_2[0], not_input_2)
        gate.NOTGate(output[0], not_output)
        gate.ANDGate3(input_1[0], not_input_2, not_output, and_1_wire)
        gate.ANDGate3(not_input_1, input_2[0], output[0], and_2_wire)
        gate.ORGate2(and_1_wire, and_2_wire, overflow)
