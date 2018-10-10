"""
This modules defines classes that simulate adders, as well as classes that are
used as building blocks for adders. These adders are designed to work only with
unsigned integers. For adder/subtractor circuits, refer to the ADD_SUB.py
module.

The following classes are defined:
    HalfAdder
    FullAdder
    LookaheadCarryUnit4
    LookaheadCarryUnit16
    Adder4
    Adder8
    Adder16
"""

from .. import wire
from .. import gate

Wire = wire.Wire
Bus4 = wire.Bus4
Bus8 = wire.Bus8
Bus16 = wire.Bus16


class HalfAdder:
    """
    This class simulates a half-adder, which has two inputs and two outputs:
                     ________
        input_1 ----|        |---- carry_out
        input_2 ----|________|---- sum

    Inputs input_1 and input_2 are both 1-bit binary numbers, and the half-
    adder computes their sum. The sum is transmitted via the sum output, and
    the carry-out is transmitted via the carry_out output.

    In general, half-adders are not very useful in building larger adders since
    they do not have a carry-in input. It is usually better to use a full-adder
    for this purpose.
    """
    def __init__(self, input_1, input_2, carry_out, sum_):
        gate.ANDGate2(input_1, input_2, carry_out)
        gate.XORGate2(input_1, input_2, sum_)


class FullAdder:
    """
    This class simulates a full-adder, which has three inputs and two outputs:
                      ________
        carry_in ----|        |---- carry_out
         input_1 ----|        |---- sum
         input_2 ----|________|

    Inputs input_1, input_2, and carry_in are all 1-bit binary numbers, and the
    half-adder computes their sum. The sum is transmitted via the sum output,
    and the carry-out is transmitted via the carry_out output.

    Because full-adders have a carry-in input, they are far more useful as the
    building blocks for larger adders than half-adders.
    """
    def __init__(self, carry_in, input_1, input_2, carry_out, sum_):
        wire_1 = Wire()
        wire_2 = Wire()
        wire_3 = Wire()

        gate.XORGate2(input_1, input_2, wire_1)
        gate.XORGate2(carry_in, wire_1, sum_)
        gate.ANDGate2(input_1, input_2, wire_2)
        gate.ANDGate2(carry_in, wire_1, wire_3)
        gate.ORGate2(wire_2, wire_3, carry_out)


class LookaheadCarryUnit4:
    """
    This class simulates a lookahead carry unit for a 4-bit adder. Though an
    LCU is not used in the 4-bit adder itself, it is useful for fast operations
    with chained 4-bit adders. An LCU takes the binary numbers being added
    together and determines if a carry is generated, without waiting for all
    the carries to "ripple" through the chain.

    This LCU has eight inputs in two 4-bit buses, a carry_in input, and three
    outputs:
                      ________
        carry_in ----|        |---- carry_out
         input_1 ----|        |---- group_propagate
         input_2 ----|        |---- group_generate
         input_3 ----|        |
         input_4 ----|        |
         input_5 ----|        |
         input_6 ----|        |
         input_7 ----|        |
         input_8 ----|________|

    Inputs input_1 and input_4 correspond to the MSB and LSB, respectively,
    of the first addend, and inputs input_5 and input_8 correspond to the MSB
    and LSB, respectively, of the second addend. The carry-in input is usually
    0 for an adder, but can be 1 if multiple adders are chained together.

    The carry_out output denotes whether or not the two 4-bit binary numbers
    will have a carry-out. The group_propagate and group_generate outputs are
    used in higher-order LCU's and denote, respectively, whether the binary
    numbers will propagate an input carry-in to carry-out, and whether the
    binary numbers will themselves generate a carry-out.
    """
    def __init__(
            self,
            carry_in,
            input_bus_1,
            input_bus_2,
            carry_out,
            group_propagate,
            group_generate
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

        input_1 = input_bus_1.wires
        input_2 = input_bus_2.wires

        G_0 = Wire()
        G_1 = Wire()
        G_2 = Wire()
        G_3 = Wire()
        P_0 = Wire()
        P_1 = Wire()
        P_2 = Wire()
        P_3 = Wire()
        wire_1 = Wire()
        wire_2 = Wire()
        wire_3 = Wire()
        wire_4 = Wire()

        gate.ANDGate2(input_1[3], input_2[3], G_0)
        gate.ANDGate2(input_1[2], input_2[2], G_1)
        gate.ANDGate2(input_1[1], input_2[1], G_2)
        gate.ANDGate2(input_1[0], input_2[0], G_3)
        gate.ORGate2(input_1[3], input_2[3], P_0)
        gate.ORGate2(input_1[2], input_2[2], P_1)
        gate.ORGate2(input_1[1], input_2[1], P_2)
        gate.ORGate2(input_1[0], input_2[0], P_3)

        gate.ANDGate4(P_0, P_1, P_2, P_3, group_propagate)
        gate.ANDGate2(G_2, P_3, wire_1)
        gate.ANDGate3(G_1, P_2, P_3, wire_2)
        gate.ANDGate4(G_0, P_1, P_2, P_3, wire_3)
        gate.ORGate4(G_3, wire_1, wire_2, wire_3, group_generate)
        gate.ANDGate2(carry_in, group_propagate, wire_4)
        gate.ORGate2(wire_4, group_generate, carry_out)


class LookaheadCarryUnit16:
    """
    This class simulates a lookahead carry unit for a 16-bit adder. It is
    useful for fast operations, since it takes the binary numbers being added
    together and determines if a carry is generated, without waiting for all
    the carries to "ripple" through the chain of adders.

    This LCU has thirty-two inputs in two 16-bit buses, a carry_in input, and
    six outputs:
                      ________
        carry_in ----|        |---- internal_carry_1
         input_1 ----|        |---- internal_carry_2
         input_2 ----|        |---- internal_carry_3
         input_3 ----|        |---- carry_out
         input_4 ----|        |---- group_propagate
             ...     |        |---- group_generate
        input_13 ----|        |
        input_14 ----|        |
        input_15 ----|        |
        input_16 ----|        |
        input_17 ----|        |
        input_18 ----|        |
        input_19 ----|        |
        input_20 ----|        |
             ...
        input_29 ----|        |
        input_30 ----|        |
        input_31 ----|        |
        input_32 ----|________|

    Inputs input_1 and input_16 correspond to the MSB and LSB, respectively,
    of the first addend, and inputs input_17 and input_32 correspond to the MSB
    and LSB, respectively, of the second addend. The carry-in input is usually
    0 for an adder, but can be 1 if multiple adders are chained together.

    The carry_out output denotes whether or not the two 16-bit binary numbers
    will have a carry-out. The internal_carry outputs are used in the 16-bit
    adder to speed up operations, with internal_carry_1 corresponding to the
    carry from the LSB to the second-LSB. The group_propagate and
    group_generate outputs are used in higher-order LCU's and denote,
    respectively, whether the binary numbers will propagate an input carry-in
    to carry-out, and whether the binary numbers will themselves generate a
    carry-out.
    """
    def __init__(
            self,
            carry_in,
            input_bus_1,
            input_bus_2,
            internal_carry_1,
            internal_carry_2,
            internal_carry_3,
            carry_out,
            group_propagate,
            group_generate
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

        input_1_bus_1 = Bus4(*input_bus_1.wires[12:16])
        input_1_bus_2 = Bus4(*input_bus_1.wires[8:12])
        input_1_bus_3 = Bus4(*input_bus_1.wires[4:8])
        input_1_bus_4 = Bus4(*input_bus_1.wires[0:4])
        input_2_bus_1 = Bus4(*input_bus_2.wires[12:16])
        input_2_bus_2 = Bus4(*input_bus_2.wires[8:12])
        input_2_bus_3 = Bus4(*input_bus_2.wires[4:8])
        input_2_bus_4 = Bus4(*input_bus_2.wires[0:4])
        lcu_1_pg = Wire()
        lcu_1_gg = Wire()
        lcu_2_pg = Wire()
        lcu_2_gg = Wire()
        lcu_3_pg = Wire()
        lcu_3_gg = Wire()
        lcu_4_pg = Wire()
        lcu_4_gg = Wire()
        wire_1 = Wire()
        wire_2 = Wire()
        wire_3 = Wire()

        LookaheadCarryUnit4(
            carry_in,
            input_1_bus_1,
            input_2_bus_1,
            internal_carry_1,
            lcu_1_pg,
            lcu_1_gg
        )
        LookaheadCarryUnit4(
            internal_carry_1,
            input_1_bus_2,
            input_2_bus_2,
            internal_carry_2,
            lcu_2_pg,
            lcu_2_gg
        )
        LookaheadCarryUnit4(
            internal_carry_2,
            input_1_bus_3,
            input_2_bus_3,
            internal_carry_3,
            lcu_3_pg,
            lcu_3_gg
        )
        LookaheadCarryUnit4(
            internal_carry_3,
            input_1_bus_4,
            input_2_bus_4,
            carry_out,
            lcu_4_pg,
            lcu_4_gg
        )

        gate.ANDGate4(lcu_1_pg, lcu_2_pg, lcu_3_pg, lcu_3_pg, group_propagate)
        gate.ANDGate2(lcu_3_gg, lcu_4_pg, wire_1)
        gate.ANDGate3(lcu_2_gg, lcu_3_pg, lcu_4_pg, wire_2)
        gate.ANDGate4(lcu_1_gg, lcu_2_pg, lcu_3_pg, lcu_4_pg, wire_3)
        gate.ORGate4(lcu_4_gg, wire_1, wire_2, wire_3, group_generate)


class Adder4:
    """
    This adder has eight inputs in two 4-bit buses, a carry_in input, a
    carry_out output, and four outputs in a single 4-bit bus:
                      ________
        carry_in ----|        |---- carry_out
         input_1 ----|        |---- output_1
         input_2 ----|        |---- output_2
         input_3 ----|        |---- output_3
         input_4 ----|        |---- output_4
         input_5 ----|        |
         input_6 ----|        |
         input_7 ----|        |
         input_8 ----|________|

    The adder computes the sum of two 4-bit binary numbers. Inputs input_1 and
    input_4 correspond to the MSB and LSB, respectively, of the first addend
    (input_bus_1), and inputs input_5 and input_8 correspond to the MSB and
    LSB, respectively, of the second addend (input_bus_2). The carry-in input
    is usually 0 for an adder, but can be 1 if multiple adders are chained
    together. The outputs have carry_out and output_4 as the MSB and LSB,
    respectively.
    """
    def __init__(
            self,
            carry_in,
            input_bus_1,
            input_bus_2,
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

        input_1 = input_bus_1.wires
        input_2 = input_bus_2.wires
        output = output_bus.wires

        carry_out_1 = Wire()
        carry_out_2 = Wire()
        carry_out_3 = Wire()

        FullAdder(carry_in, input_1[3], input_2[3], carry_out_1, output[3])
        FullAdder(carry_out_1, input_1[2], input_2[2], carry_out_2, output[2])
        FullAdder(carry_out_2, input_1[1], input_2[1], carry_out_3, output[1])
        FullAdder(carry_out_3, input_1[0], input_2[0], carry_out, output[0])


class Adder8:
    """
    This adder has sixteen inputs in two 8-bit buses, a carry_in input, a
    carry_out output, and eight outputs in a single 8-bit bus:
                      ________
        carry_in ----|        |---- carry_out
         input_1 ----|        |---- output_1
         input_2 ----|        |---- output_2
         input_3 ----|        |---- output_3
         input_4 ----|        |---- output_4
         input_5 ----|        |---- output_5
         input_6 ----|        |---- output_6
         input_7 ----|        |---- output_7
         input_8 ----|        |---- output_8
         input_9 ----|        |
        input_10 ----|        |
        input_11 ----|        |
        input_12 ----|        |
        input_13 ----|        |
        input_14 ----|        |
        input_15 ----|        |
        input_16 ----|________|

    The adder computes the sum of two 8-bit binary numbers. Inputs input_1 and
    input_8 correspond to the MSB and LSB, respectively, of the first addend
    (input_bus_1), and inputs input_9 and input_16 correspond to the MSB and
    LSB, respectively, of the second addend (input_bus_2). The carry-in input
    is usually 0 for an adder, but can be 1 if multiple adders are chained
    together. The outputs  have carry_out and output_8 as the MSB and LSB,
    respectively.
    """
    def __init__(
            self,
            carry_in,
            input_bus_1,
            input_bus_2,
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

        input_1 = input_bus_1.wires
        input_2 = input_bus_2.wires
        output = output_bus.wires
        input_1_1 = Bus4(*input_1[0:4])
        input_1_2 = Bus4(*input_1[4:8])
        input_2_1 = Bus4(*input_2[0:4])
        input_2_2 = Bus4(*input_2[4:8])
        output_1 = Bus4(*output[0:4])
        output_2 = Bus4(*output[4:8])

        lcu_c = Wire()
        lcu_pg = Wire()
        lcu_gg = Wire()

        LookaheadCarryUnit4(
            carry_in,
            input_1_2,
            input_2_2,
            lcu_c,
            lcu_pg,
            lcu_gg
        )
        Adder4(lcu_c, input_1_1, input_2_1, carry_out, output_1)
        Adder4(carry_in, input_1_2, input_2_2, lcu_c, output_2)


class Adder16:
    """
    This adder has thirty-two inputs in two 16-bit buses, a carry_in input, a
    carry_out output, and sixteen outputs in a single 16-bit bus:
                      ________
        carry_in ----|        |---- carry_out
         input_1 ----|        |---- output_1
         input_2 ----|        |---- output_2
         input_3 ----|        |---- output_3
         input_4 ----|        |---- output_4
             ...     |        |---- output_5
        input_13 ----|        |---- output_6
        input_14 ----|        |---- output_7
        input_15 ----|        |---- output_8
        input_16 ----|        |---- output_9
        input_17 ----|        |---- output_10
        input_18 ----|        |---- output_11
        input_19 ----|        |---- output_12
        input_20 ----|        |---- output_13
             ...     |        |---- output_14
        input_29 ----|        |---- output_15
        input_30 ----|        |---- output_16
        input_31 ----|        |
        input_32 ----|________|

    The adder computes the sum of two 16-bit binary numbers. Inputs input_1 and
    input_16 correspond to the MSB and LSB, respectively, of the first addend
    (input_bus_1), and inputs input_17 and input_32 correspond to the MSB and
    LSB, respectively, of the second addend (input_bus_2). The carry-in input
    is usually 0 for an adder, but can be 1 if multiple adders are chained
    together. The outputs have carry_out and output_16 as the MSB and LSB,
    respectively.
    """
    def __init__(
            self,
            carry_in,
            input_bus_1,
            input_bus_2,
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

        input_1 = input_bus_1.wires
        input_2 = input_bus_2.wires
        output = output_bus.wires
        input_1_1 = Bus4(*input_1[0:4])
        input_1_2 = Bus4(*input_1[4:8])
        input_1_3 = Bus4(*input_1[8:12])
        input_1_4 = Bus4(*input_1[12:16])
        input_2_1 = Bus4(*input_2[0:4])
        input_2_2 = Bus4(*input_2[4:8])
        input_2_3 = Bus4(*input_2[8:12])
        input_2_4 = Bus4(*input_2[12:16])
        output_1 = Bus4(*output[0:4])
        output_2 = Bus4(*output[4:8])
        output_3 = Bus4(*output[8:12])
        output_4 = Bus4(*output[12:16])

        ic_1 = Wire()
        ic_2 = Wire()
        ic_3 = Wire()
        lcu_pg = Wire()
        lcu_gg = Wire()

        LookaheadCarryUnit16(
            carry_in,
            input_bus_1,
            input_bus_2,
            ic_1,
            ic_2,
            ic_3,
            carry_out,
            lcu_pg,
            lcu_gg
        )

        Adder4(ic_3, input_1_1, input_2_1, carry_out, output_1)
        Adder4(ic_2, input_1_2, input_2_2, ic_3, output_2)
        Adder4(ic_1, input_1_3, input_2_3, ic_2, output_3)
        Adder4(carry_in, input_1_4, input_2_4, ic_1, output_4)
