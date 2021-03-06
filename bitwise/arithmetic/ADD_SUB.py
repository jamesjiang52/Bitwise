"""
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
    """Construct a new 4-bit adder-subtractor.

    Args:
        add_subtract: An object of type Wire. Indicates the operation to carry
            out - 0 for addition, 1 for subtraction.
        a_bus: An object of type Bus4. The first addend, or the minuend.
            a_bus[0] and a_bus[3] are the most and least significant bit,
            respectively. a_bus[0] is the sign bit in subtraction operations.
        b_bus: An object of type Bus4. The second addend, or the subtrahend.
            b_bus[0] and b_bus[3] are the most and least significant bit,
            respectively. b_bus[0] is the sign bit in subtraction operations.
        overflow: An object of type Wire. The overflow indicator of the
            subtractor.
        carry_out: An object of type Wire. The carry-out of the adder.
        sum_bus: An object of type Bus4. The sum of the two addends, or the
            difference between the minuend and the subtrahend. sum_bus[0] and
            sum_bus[3] are the most and least significant bit, respectively.
            sum_bus[0] is the sign bit in subtraction operations.

    Raises:
        TypeError: If either a_bus, b_bus, or sum_bus is not a bus of width 4.
    """
    def __init__(
        self,
        add_subtract,
        a_bus,
        b_bus,
        overflow,
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
        input_1 = a_bus.wires
        input_2 = b_bus.wires
        output = sum_bus.wires

        signal.ControlledInverter4(add_subtract, b_bus, bus_1)
        ADD.Adder4(add_subtract, a_bus, bus_1, carry_out, sum_bus)

        gate.NOTGate(input_1[0], not_input_1)
        gate.NOTGate(input_2[0], not_input_2)
        gate.NOTGate(output[0], not_output)
        gate.ANDGate3(input_1[0], not_input_2, not_output, and_1_wire)
        gate.ANDGate3(not_input_1, input_2[0], output[0], and_2_wire)
        gate.ORGate2(and_1_wire, and_2_wire, overflow)

        self.add_subtract = add_subtract
        self.a_bus = a_bus
        self.b_bus = b_bus
        self.overflow = overflow
        self.carry_out = carry_out
        self.sum_bus = sum_bus

    def __str__(self):
        str_ = ""
        str_ += "add_subtract: " + str(self.add_subtract.value) + "\n"
        str_ += "a_bus: " + self.a_bus.__str__() + "\n"
        str_ += "b_bus: " + self.b_bus.__str__() + "\n"
        str_ += "overflow: " + str(self.overflow.value) + "\n"
        str_ += "carry_out: " + str(self.carry_out.value) + "\n"
        str_ += "sum_bus: " + self.sum_bus.__str__()
        return str_

    def __call__(
        self, *,
        add_subtract=None,
        a_bus=None,
        b_bus=None,
        overflow=None,
        carry_out=None,
        sum_bus=None
    ):
        if add_subtract is not None:
            self.add_subtract.value = add_subtract
        if a_bus is not None:
            self.a_bus.wire_values = a_bus
        if b_bus is not None:
            self.b_bus.wire_values = b_bus
        if overflow is not None:
            self.overflow.value = overflow
        if carry_out is not None:
            self.carry_out.value = carry_out
        if sum_bus is not None:
            self.sum_bus.wire_values = sum_bus


class AdderSubtractor8:
    """Construct a new 8-bit adder-subtractor.

    Args:
        add_subtract: An object of type Wire. Indicates the operation to carry
            out - 0 for addition, 1 for subtraction.
        a_bus: An object of type Bus8. The first addend, or the minuend.
            a_bus[0] and a_bus[7] are the most and least significant bit,
            respectively. a_bus[0] is the sign bit in subtraction operations.
        b_bus: An object of type Bus8. The second addend, or the subtrahend.
            b_bus[0] and b_bus[7] are the most and least significant bit,
            respectively. b_bus[0] is the sign bit in subtraction operations.
        overflow: An object of type Wire. The overflow indicator of the
            subtractor.
        carry_out: An object of type Wire. The carry-out of the adder.
        sum_bus: An object of type Bus8. The sum of the two addends, or the
            difference between the minuend and the subtrahend. sum_bus[0] and
            sum_bus[7] are the most and least significant bit, respectively.
            sum_bus[0] is the sign bit in subtraction operations.

    Raises:
        TypeError: If either a_bus, b_bus, or sum_bus is not a bus of width 8.
    """
    def __init__(
        self,
        add_subtract,
        a_bus,
        b_bus,
        overflow,
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
        input_1 = a_bus.wires
        input_2 = b_bus.wires
        output = sum_bus.wires

        signal.ControlledInverter8(add_subtract, b_bus, bus_1)
        ADD.Adder8(add_subtract, a_bus, bus_1, carry_out, sum_bus)

        gate.NOTGate(input_1[0], not_input_1)
        gate.NOTGate(input_2[0], not_input_2)
        gate.NOTGate(output[0], not_output)
        gate.ANDGate3(input_1[0], not_input_2, not_output, and_1_wire)
        gate.ANDGate3(not_input_1, input_2[0], output[0], and_2_wire)
        gate.ORGate2(and_1_wire, and_2_wire, overflow)

        self.add_subtract = add_subtract
        self.a_bus = a_bus
        self.b_bus = b_bus
        self.overflow = overflow
        self.carry_out = carry_out
        self.sum_bus = sum_bus

    def __str__(self):
        str_ = ""
        str_ += "add_subtract: " + str(self.add_subtract.value) + "\n"
        str_ += "a_bus: " + self.a_bus.__str__() + "\n"
        str_ += "b_bus: " + self.b_bus.__str__() + "\n"
        str_ += "overflow: " + str(self.overflow.value) + "\n"
        str_ += "carry_out: " + str(self.carry_out.value) + "\n"
        str_ += "sum_bus: " + self.sum_bus.__str__()
        return str_

    def __call__(
        self, *,
        add_subtract=None,
        a_bus=None,
        b_bus=None,
        overflow=None,
        carry_out=None,
        sum_bus=None
    ):
        if add_subtract is not None:
            self.add_subtract.value = add_subtract
        if a_bus is not None:
            self.a_bus.wire_values = a_bus
        if b_bus is not None:
            self.b_bus.wire_values = b_bus
        if overflow is not None:
            self.overflow.value = overflow
        if carry_out is not None:
            self.carry_out.value = carry_out
        if sum_bus is not None:
            self.sum_bus.wire_values = sum_bus


class AdderSubtractor16:
    """Construct a new 16-bit adder-subtractor.

    Args:
        add_subtract: An object of type Wire. Indicates the operation to carry
            out - 0 for addition, 1 for subtraction.
        a_bus: An object of type Bus16. The first addend, or the minuend.
            a_bus[0] and a_bus[15] are the most and least significant bit,
            respectively. a_bus[0] is the sign bit in subtraction operations.
        b_bus: An object of type Bus16. The second addend, or the subtrahend.
            b_bus[0] and b_bus[15] are the most and least significant bit,
            respectively. b_bus[0] is the sign bit in subtraction operations.
        overflow: An object of type Wire. The overflow indicator of the
            subtractor.
        carry_out: An object of type Wire. The carry-out of the adder.
        sum_bus: An object of type Bus16. The sum of the two addends, or the
            difference between the minuend and the subtrahend. sum_bus[0] and
            sum_bus[15] are the most and least significant bit, respectively.
            sum_bus[0] is the sign bit in subtraction operations.

    Raises:
        TypeError: If either a_bus, b_bus, or sum_bus is not a bus of width 16.
    """
    def __init__(
        self,
        add_subtract,
        a_bus,
        b_bus,
        overflow,
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
        input_1 = a_bus.wires
        input_2 = b_bus.wires
        output = sum_bus.wires

        signal.ControlledInverter16(add_subtract, b_bus, bus_1)
        ADD.Adder16(add_subtract, a_bus, bus_1, carry_out, sum_bus)

        gate.NOTGate(input_1[0], not_input_1)
        gate.NOTGate(input_2[0], not_input_2)
        gate.NOTGate(output[0], not_output)
        gate.ANDGate3(input_1[0], not_input_2, not_output, and_1_wire)
        gate.ANDGate3(not_input_1, input_2[0], output[0], and_2_wire)
        gate.ORGate2(and_1_wire, and_2_wire, overflow)

        self.add_subtract = add_subtract
        self.a_bus = a_bus
        self.b_bus = b_bus
        self.overflow = overflow
        self.carry_out = carry_out
        self.sum_bus = sum_bus

    def __str__(self):
        str_ = ""
        str_ += "add_subtract: " + str(self.add_subtract.value) + "\n"
        str_ += "a_bus: " + self.a_bus.__str__() + "\n"
        str_ += "b_bus: " + self.b_bus.__str__() + "\n"
        str_ += "overflow: " + str(self.overflow.value) + "\n"
        str_ += "carry_out: " + str(self.carry_out.value) + "\n"
        str_ += "sum_bus: " + self.sum_bus.__str__()
        return str_

    def __call__(
        self, *,
        add_subtract=None,
        a_bus=None,
        b_bus=None,
        overflow=None,
        carry_out=None,
        sum_bus=None
    ):
        if add_subtract is not None:
            self.add_subtract.value = add_subtract
        if a_bus is not None:
            self.a_bus.wire_values = a_bus
        if b_bus is not None:
            self.b_bus.wire_values = b_bus
        if overflow is not None:
            self.overflow.value = overflow
        if carry_out is not None:
            self.carry_out.value = carry_out
        if sum_bus is not None:
            self.sum_bus.wire_values = sum_bus
