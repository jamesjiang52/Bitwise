"""
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
    """Construct a new 3-bit logical comparator.

    Args:
        a_bus: An object of type Bus4. The number to be compared. a_bus[1] and
            a_bus[3] are the most and least significant bit, respectively.
            a_bus[0] is the sign bit.
        b_bus: An object of type Bus4. The number to be compared against.
            b_bus[1] and b_bus[3] are the most and least significant bit,
            respectively. b_bus[0] is the sign bit.
        greater_than: An object of type Wire. The greater-than indicator.
        equal_to: An object of type Wire. The equal-to indicator.
        less_than: An object of type Wire. The less-than indicator.

    Raises:
        TypeError: If either a_bus or b_bus is not a bus of width 4.
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

        self.a_bus = input_bus_1
        self.b_bus = input_bus_2
        self.greater_than = gt
        self.equal_to = z
        self.less_than = lt

    def __str__(self):
        str_ = ""
        str_ += "a_bus: " + self.a_bus.__str__() + "\n"
        str_ += "b_bus: " + self.b_bus.__str__() + "\n"
        str_ += "greater_than: " + str(self.greater_than.value) + "\n"
        str_ += "equal_to: " + str(self.equal_to.value) + "\n"
        str_ += "less_than: " + str(self.less_than.value)
        return str_

    def __call__(
        self, *,
        a_bus=None,
        b_bus=None,
        greater_than=None,
        equal_to=None,
        less_than=None
    ):
        if a_bus is not None:
            self.a_bus.wire_values = a_bus
        if b_bus is not None:
            self.b_bus.wire_values = b_bus
        if greater_than is not None:
            self.greater_than.value = greater_than
        if equal_to is not None:
            self.equal_to.value = equal_to
        if less_than is not None:
            self.less_than.value = less_than


class Comparator7:
    """Construct a new 7-bit logical comparator.

    Args:
        a_bus: An object of type Bus8. The number to be compared. a_bus[1] and
            a_bus[7] are the most and least significant bit, respectively.
            a_bus[0] is the sign bit.
        b_bus: An object of type Bus8. The number to be compared against.
            b_bus[1] and b_bus[7] are the most and least significant bit,
            respectively. b_bus[0] is the sign bit.
        greater_than: An object of type Wire. The greater-than indicator.
        equal_to: An object of type Wire. The equal-to indicator.
        less_than: An object of type Wire. The less-than indicator.

    Raises:
        TypeError: If either a_bus or b_bus is not a bus of width 8.
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

        self.a_bus = input_bus_1
        self.b_bus = input_bus_2
        self.greater_than = gt
        self.equal_to = z
        self.less_than = lt

    def __str__(self):
        str_ = ""
        str_ += "a_bus: " + self.a_bus.__str__() + "\n"
        str_ += "b_bus: " + self.b_bus.__str__() + "\n"
        str_ += "greater_than: " + str(self.greater_than.value) + "\n"
        str_ += "equal_to: " + str(self.equal_to.value) + "\n"
        str_ += "less_than: " + str(self.less_than.value)
        return str_

    def __call__(
        self, *,
        a_bus=None,
        b_bus=None,
        greater_than=None,
        equal_to=None,
        less_than=None
    ):
        if a_bus is not None:
            self.a_bus.wire_values = a_bus
        if b_bus is not None:
            self.b_bus.wire_values = b_bus
        if greater_than is not None:
            self.greater_than.value = greater_than
        if equal_to is not None:
            self.equal_to.value = equal_to
        if less_than is not None:
            self.less_than.value = less_than


class Comparator15:
    """Construct a new 15-bit logical comparator.

    Args:
        a_bus: An object of type Bus16. The number to be compared. a_bus[1] and
            a_bus[15] are the most and least significant bit, respectively.
            a_bus[0] is the sign bit.
        b_bus: An object of type Bus16. The number to be compared against.
            b_bus[1] and b_bus[15] are the most and least significant bit,
            respectively. b_bus[0] is the sign bit.
        greater_than: An object of type Wire. The greater-than indicator.
        equal_to: An object of type Wire. The equal-to indicator.
        less_than: An object of type Wire. The less-than indicator.

    Raises:
        TypeError: If either a_bus or b_bus is not a bus of width 16.
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

        self.a_bus = input_bus_1
        self.b_bus = input_bus_2
        self.greater_than = gt
        self.equal_to = z
        self.less_than = lt

    def __str__(self):
        str_ = ""
        str_ += "a_bus: " + self.a_bus.__str__() + "\n"
        str_ += "b_bus: " + self.b_bus.__str__() + "\n"
        str_ += "greater_than: " + str(self.greater_than.value) + "\n"
        str_ += "equal_to: " + str(self.equal_to.value) + "\n"
        str_ += "less_than: " + str(self.less_than.value)
        return str_

    def __call__(
        self, *,
        a_bus=None,
        b_bus=None,
        greater_than=None,
        equal_to=None,
        less_than=None
    ):
        if a_bus is not None:
            self.a_bus.wire_values = a_bus
        if b_bus is not None:
            self.b_bus.wire_values = b_bus
        if greater_than is not None:
            self.greater_than.value = greater_than
        if equal_to is not None:
            self.equal_to.value = equal_to
        if less_than is not None:
            self.less_than.value = less_than
