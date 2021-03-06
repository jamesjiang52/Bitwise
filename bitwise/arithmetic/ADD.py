"""
The following classes are defined:
    Adder4
    Adder8
    Adder16
    FullAdder
    HalfAdder
"""

from .. import wire
from .. import gate

Wire = wire.Wire
Bus4 = wire.Bus4
Bus8 = wire.Bus8
Bus16 = wire.Bus16


class HalfAdder:
    """Construct a new half adder.

    Args:
        a: An object of type Wire. The first addend.
        b: An object of type Wire. The second addend.
        carry_out: An object of type Wire. The carry-out of the adder.
        sum: An object of type Wire. The sum of the two addends.
    """
    def __init__(self, a, b, carry_out, sum):
        gate.ANDGate2(a, b, carry_out)
        gate.XORGate2(a, b, sum)

        self.a = a
        self.b = b
        self.carry_out = carry_out
        self.sum = sum

    def __str__(self):
        str_ = ""
        str_ += "a: " + str(self.a.value) + "\n"
        str_ += "b: " + str(self.b.value) + "\n"
        str_ += "carry_out: " + str(self.carry_out.value) + "\n"
        str_ += "sum: " + str(self.sum.value)
        return str_

    def __call__(
        self, *,
        a=None,
        b=None,
        carry_out=None,
        sum=None
    ):
        if a is not None:
            self.a.value = a
        if b is not None:
            self.b.value = b
        if carry_out is not None:
            self.carry_out.value = carry_out
        if sum is not None:
            self.sum.value = sum


class FullAdder:
    """Construct a new full adder.

    Args:
        carry_in: An object of type Wire. The carry-in to the adder.
        a: An object of type Wire. The first addend.
        b: An object of type Wire. The second addend.
        carry_out: An object of type Wire. The carry-out of the adder.
        sum: An object of type Wire. The sum of the two addends.
    """
    def __init__(self, carry_in, a, b, carry_out, sum):
        wire_1 = Wire()
        wire_2 = Wire()
        wire_3 = Wire()

        gate.XORGate2(a, b, wire_1)
        gate.XORGate2(carry_in, wire_1, sum)
        gate.ANDGate2(a, b, wire_2)
        gate.ANDGate2(carry_in, wire_1, wire_3)
        gate.ORGate2(wire_2, wire_3, carry_out)

        self.carry_in = carry_in
        self.a = a
        self.b = b
        self.carry_out = carry_out
        self.sum = sum

    def __str__(self):
        str_ = ""
        str_ += "carry_in: " + str(self.carry_in.value) + "\n"
        str_ += "a: " + str(self.a.value) + "\n"
        str_ += "b: " + str(self.b.value) + "\n"
        str_ += "carry_out: " + str(self.carry_out.value) + "\n"
        str_ += "sum: " + str(self.sum.value)
        return str_

    def __call__(
        self, *,
        carry_in=None,
        a=None,
        b=None,
        carry_out=None,
        sum=None
    ):
        if carry_in is not None:
            self.carry_in.value = carry_in
        if a is not None:
            self.a.value = a
        if b is not None:
            self.b.value = b
        if carry_out is not None:
            self.carry_out.value = carry_out
        if sum is not None:
            self.sum.value = sum


class _LookaheadCarryUnit4:
    """               ________
        carry_in ----|        |---- carry_out
         input_1 ----|        |---- group_propagate
         input_2 ----|        |---- group_generate
         input_3 ----|        |
         input_4 ----|        |
         input_5 ----|        |
         input_6 ----|        |
         input_7 ----|        |
         input_8 ----|________|
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


class _LookaheadCarryUnit16:
    """               ________
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

        _LookaheadCarryUnit4(
            carry_in,
            input_1_bus_1,
            input_2_bus_1,
            internal_carry_1,
            lcu_1_pg,
            lcu_1_gg
        )
        _LookaheadCarryUnit4(
            internal_carry_1,
            input_1_bus_2,
            input_2_bus_2,
            internal_carry_2,
            lcu_2_pg,
            lcu_2_gg
        )
        _LookaheadCarryUnit4(
            internal_carry_2,
            input_1_bus_3,
            input_2_bus_3,
            internal_carry_3,
            lcu_3_pg,
            lcu_3_gg
        )
        _LookaheadCarryUnit4(
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
    """Construct a new 4-bit adder.

    Args:
        carry_in: An object of type Wire. The carry-in to the adder.
        a_bus: An object of type Bus4. The first addend. a_bus[0] and a_bus[3]
            are the most and least significant bit, respectively.
        b_bus: An object of type Bus4. The second addend. b_bus[0] and b_bus[3]
            are the most and least significant bit, respectively.
        carry_out: An object of type Wire. The carry-out of the adder.
        sum_bus: An object of type Bus4. The sum of the two addends. sum_bus[0]
            and sum_bus[3] are the most and least significant bit,
            respectively.

    Raises:
        TypeError: If either a_bus, b_bus, or sum_bus is not a bus of width 4.
    """
    def __init__(
        self,
        carry_in,
        a_bus,
        b_bus,
        carry_out,
        sum_bus
    ):
        if len(a_bus.wires) != 4:
            raise TypeError(
                "Expected bus of width 4, received bus of width {0}.".format(
                    len(a_bus.wires)
                )
            )

        if len(b_bus.wires) != 4:
            raise TypeError(
                "Expected bus of width 4, received bus of width {0}.".format(
                    len(b_bus.wires)
                )
            )

        if len(sum_bus.wires) != 4:
            raise TypeError(
                "Expected bus of width 4, received bus of width {0}.".format(
                    len(sum_bus.wires)
                )
            )

        input_1 = a_bus.wires
        input_2 = b_bus.wires
        output = sum_bus.wires

        carry_out_1 = Wire()
        carry_out_2 = Wire()
        carry_out_3 = Wire()

        FullAdder(carry_in, input_1[3], input_2[3], carry_out_1, output[3])
        FullAdder(carry_out_1, input_1[2], input_2[2], carry_out_2, output[2])
        FullAdder(carry_out_2, input_1[1], input_2[1], carry_out_3, output[1])
        FullAdder(carry_out_3, input_1[0], input_2[0], carry_out, output[0])

        self.carry_in = carry_in
        self.a_bus = a_bus
        self.b_bus = b_bus
        self.carry_out = carry_out
        self.sum_bus = sum_bus

    def __str__(self):
        str_ = ""
        str_ += "carry_in: " + str(self.carry_in.value) + "\n"
        str_ += "a_bus: " + self.a_bus.__str__() + "\n"
        str_ += "b_bus: " + self.b_bus.__str__() + "\n"
        str_ += "carry_out: " + str(self.carry_out.value) + "\n"
        str_ += "sum_bus: " + self.sum_bus.__str__()
        return str_

    def __call__(
        self, *,
        carry_in=None,
        a_bus=None,
        b_bus=None,
        carry_out=None,
        sum_bus=None
    ):
        if carry_in is not None:
            self.carry_in.value = carry_in
        if a_bus is not None:
            self.a_bus.wire_values = a_bus
        if b_bus is not None:
            self.b_bus.wire_values = b_bus
        if carry_out is not None:
            self.carry_out.value = carry_out
        if sum_bus is not None:
            self.sum_bus.wire_values = sum_bus


class Adder8:
    """Construct a new 8-bit adder.

    Args:
        carry_in: An object of type Wire. The carry-in to the adder.
        a_bus: An object of type Bus8. The first addend. a_bus[0] and a_bus[7]
            are the most and least significant bit, respectively.
        b_bus: An object of type Bus8. The second addend. b_bus[0] and b_bus[7]
            are the most and least significant bit, respectively.
        carry_out: An object of type Wire. The carry-out of the adder.
        sum_bus: An object of type Bus8. The sum of the two addends. sum_bus[0]
            and sum_bus[7] are the most and least significant bit,
            respectively.

    Raises:
        TypeError: If either a_bus, b_bus, or sum_bus is not a bus of width 8.
    """
    def __init__(
        self,
        carry_in,
        a_bus,
        b_bus,
        carry_out,
        sum_bus
    ):
        if len(a_bus.wires) != 8:
            raise TypeError(
                "Expected bus of width 8, received bus of width {0}.".format(
                    len(a_bus.wires)
                )
            )

        if len(b_bus.wires) != 8:
            raise TypeError(
                "Expected bus of width 8, received bus of width {0}.".format(
                    len(b_bus.wires)
                )
            )

        if len(sum_bus.wires) != 8:
            raise TypeError(
                "Expected bus of width 8, received bus of width {0}.".format(
                    len(sum_bus.wires)
                )
            )

        input_1 = a_bus.wires
        input_2 = b_bus.wires
        output = sum_bus.wires
        input_1_1 = Bus4(*input_1[0:4])
        input_1_2 = Bus4(*input_1[4:8])
        input_2_1 = Bus4(*input_2[0:4])
        input_2_2 = Bus4(*input_2[4:8])
        output_1 = Bus4(*output[0:4])
        output_2 = Bus4(*output[4:8])

        lcu_c = Wire()
        lcu_pg = Wire()
        lcu_gg = Wire()

        _LookaheadCarryUnit4(
            carry_in,
            input_1_2,
            input_2_2,
            lcu_c,
            lcu_pg,
            lcu_gg
        )
        Adder4(lcu_c, input_1_1, input_2_1, carry_out, output_1)
        Adder4(carry_in, input_1_2, input_2_2, lcu_c, output_2)

        self.carry_in = carry_in
        self.a_bus = a_bus
        self.b_bus = b_bus
        self.carry_out = carry_out
        self.sum_bus = sum_bus

    def __str__(self):
        str_ = ""
        str_ += "carry_in: " + str(self.carry_in.value) + "\n"
        str_ += "a_bus: " + self.a_bus.__str__() + "\n"
        str_ += "b_bus: " + self.b_bus.__str__() + "\n"
        str_ += "carry_out: " + str(self.carry_out.value) + "\n"
        str_ += "sum_bus: " + self.sum_bus.__str__()
        return str_

    def __call__(
        self, *,
        carry_in=None,
        a_bus=None,
        b_bus=None,
        carry_out=None,
        sum_bus=None
    ):
        if carry_in is not None:
            self.carry_in.value = carry_in
        if a_bus is not None:
            self.a_bus.wire_values = a_bus
        if b_bus is not None:
            self.b_bus.wire_values = b_bus
        if carry_out is not None:
            self.carry_out.value = carry_out
        if sum_bus is not None:
            self.sum_bus.wire_values = sum_bus


class Adder16:
    """Construct a new 16-bit adder.

    Args:
        carry_in: An object of type Wire. The carry-in to the adder.
        a_bus: An object of type Bus16. The first addend. a_bus[0] and
            a_bus[15] are the most and least significant bit, respectively.
        b_bus: An object of type Bus16. The second addend. b_bus[0] and
            b_bus[15] are the most and least significant bit, respectively.
        carry_out: An object of type Wire. The carry-out of the adder.
        sum_bus: An object of type Bus16. The sum of the two addends.
            sum_bus[0] and sum_bus[15] are the most and least significant bit,
            respectively.

    Raises:
        TypeError: If either a_bus, b_bus, or sum_bus is not a bus of width 16.
    """
    def __init__(
        self,
        carry_in,
        a_bus,
        b_bus,
        carry_out,
        sum_bus
    ):
        if len(a_bus.wires) != 16:
            raise TypeError(
                "Expected bus of width 16, received bus of width {0}.".format(
                    len(a_bus.wires)
                )
            )

        if len(b_bus.wires) != 16:
            raise TypeError(
                "Expected bus of width 16, received bus of width {0}.".format(
                    len(b_bus.wires)
                )
            )

        if len(sum_bus.wires) != 16:
            raise TypeError(
                "Expected bus of width 16, received bus of width {0}.".format(
                    len(sum_bus.wires)
                )
            )

        input_1 = a_bus.wires
        input_2 = b_bus.wires
        output = sum_bus.wires
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

        _LookaheadCarryUnit16(
            carry_in,
            a_bus,
            b_bus,
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

        self.carry_in = carry_in
        self.a_bus = a_bus
        self.b_bus = b_bus
        self.carry_out = carry_out
        self.sum_bus = sum_bus

    def __str__(self):
        str_ = ""
        str_ += "carry_in: " + str(self.carry_in.value) + "\n"
        str_ += "a_bus: " + self.a_bus.__str__() + "\n"
        str_ += "b_bus: " + self.b_bus.__str__() + "\n"
        str_ += "carry_out: " + str(self.carry_out.value) + "\n"
        str_ += "sum_bus: " + self.sum_bus.__str__()
        return str_

    def __call__(
        self, *,
        carry_in=None,
        a_bus=None,
        b_bus=None,
        carry_out=None,
        sum_bus=None
    ):
        if carry_in is not None:
            self.carry_in.value = carry_in
        if a_bus is not None:
            self.a_bus.wire_values = a_bus
        if b_bus is not None:
            self.b_bus.wire_values = b_bus
        if carry_out is not None:
            self.carry_out.value = carry_out
        if sum_bus is not None:
            self.sum_bus.wire_values = sum_bus
