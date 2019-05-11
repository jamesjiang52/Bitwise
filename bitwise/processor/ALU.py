"""
The following classes are defined:
    ArithmeticLogicUnit
"""

from .. import wire
from .. import arithmetic
from .. import logic
from .. import signal

Wire = wire.Wire
Bus4 = wire.Bus4
Bus8 = wire.Bus8
Bus16 = wire.Bus16


class ArithmeticLogicUnit:
    """Construct a new 16-bit arithmetic-logic unit with the following
    functions:
        0000: a
        0001: NOT a
        0010: b
        0011: NOT b
        0100: a AND b
        0101: a NAND b
        0110: a OR b
        0111: a NOR b
        1000: a XOR b
        1001: a XNOR b
        1010: a PLUS b
        1011: NOT (a PLUS b)
        1100: a MINUS b
        1101: NOT (a MINUS b)
        1110: 0
        1111: 1

    Args:
        a_bus: An object of type Bus16. The first input to the ALU. The first
            addend in add operations and the minuend in subtract operations.
            Also the number to be compared. a_bus[0] and a_bus[15] are the most
            and least significant bit, respectively.
        b_bus: An object of type Bus16. The second input to the ALU. The second
            addend in add operations and the subtrahend in subtract operations.
            Also the number to be compared against. b_bus[0] and b_bus[15] are
            the most and least significant bit, respectively.
        function_select_bus: An object of type Bus4. The function select input
            of the ALU, with functions defined above. function_select_bus[0]
            and function_select_bus[3] are the most and least significant bit,
            respectively.
        overflow: An object of type Wire. The arithmetic overflow indicator.
            Only valid for functions 1100 and 1101 (subtract operations).
        carry_out: An object of type Wire. The carry-out. Only valid for
            functions 1010 and 1011 (add operations).
        output_bus: An object of type Bus16. The output of the ALU.
            output_bus[0] and output_bus[15] are the most and least significant
            bit, respectively.

    Raises:
        TypeError: If either a_bus, b_bus, or output_bus is not a bus of width
            16, or if function_select_bus is not a bus of width 4.
    """
    def __init__(
        self,
        a_bus,
        b_bus,
        function_select_bus,
        overflow,
        carry_out,
        output_bus
    ):
        if len(a_bus) != 16:
            raise TypeError(
                "Expected bus of width 16, received bus of width {0}.".format(
                    len(a_bus)
                )
            )

        if len(b_bus) != 16:
            raise TypeError(
                "Expected bus of width 16, received bus of width {0}.".format(
                    len(b_bus)
                )
            )

        if len(output_bus) != 16:
            raise TypeError(
                "Expected bus of width 16, received bus of width {0}.".format(
                    len(output_bus)
                )
            )

        if len(function_select_bus) != 4:
            raise TypeError(
                "Expected bus of width 4, received bus of width {0}.".format(
                    len(function_select_bus)
                )
            )

        a_and_b_bus = Bus16()
        a_or_b_bus = Bus16()
        a_xor_b_bus = Bus16()
        a_plus_b_bus = Bus16()
        zero_bus = Bus16()

        true_bus = Bus16()
        not_bus = Bus16()

        _Multiplexer8To1_16(
            function_select_bus[0],
            function_select_bus[1],
            function_select_bus[2],
            zero_bus,
            a_plus_b_bus,
            a_plus_b_bus,
            a_xor_b_bus,
            a_or_b_bus,
            a_and_b_bus,
            b_bus,
            a_bus,
            true_bus
        )

        _Multiplexer2To1_16(
            function_select_bus[3],
            not_bus,
            true_bus,
            output_bus
        )

        logic.BitwiseNOT16(true_bus, not_bus)

        logic.BitwiseAND16(a_bus, b_bus, a_and_b_bus)
        logic.BitwiseOR16(a_bus, b_bus, a_or_b_bus)
        logic.BitwiseXOR16(a_bus, b_bus, a_xor_b_bus)

        arithmetic.AdderSubtractor16(
            function_select_bus[1],
            a_bus,
            b_bus,
            overflow,
            carry_out,
            a_plus_b_bus
        )

        self.a_bus = a_bus
        self.b_bus = b_bus
        self.function_select_bus = function_select_bus
        self.overflow = overflow
        self.carry_out = carry_out
        self.output_bus = output_bus

    def __str__(self):
        str_ = ""
        str_ += "a_bus: " + self.a_bus.__str__() + "\n"
        str_ += "b_bus: " + self.b_bus.__str__() + "\n"
        str_ += "function_select_bus: " + self.function_select_bus.__str__() \
                + "\n"
        str_ += "overflow: " + str(self.overflow.value) + "\n"
        str_ += "carry_out: " + str(self.carry_out.value) + "\n"
        str_ += "output_bus: " + self.output_bus.__str__()
        return str_

    def __call__(
        self, *,
        a_bus=None,
        b_bus=None,
        function_select_bus=None,
        overflow=None,
        carry_out=None,
        output_bus=None
    ):
        if a_bus is not None:
            self.a_bus.wire_values = a_bus
        if b_bus is not None:
            self.b_bus.wire_values = b_bus
        if function_select_bus is not None:
            self.function_select_bus.wire_values = function_select_bus
        if overflow is not None:
            self.overflow.value = overflow
        if carry_out is not None:
            self.carry_out.value = carry_out
        if output_bus is not None:
            self.output_bus.wire_values = output_bus


class _Multiplexer2To1_16:
    """
    This is an internal module for the ALU. It multiplexes two 16-bit inputs to
    a single 16-bit output.
    """
    def __init__(
        self,
        select,
        input_1_bus,
        input_2_bus,
        output_bus
    ):
        vcc = Wire()
        vcc.value = 1

        signal.Multiplexer2To1(
            vcc,
            select,
            input_1_bus[0],
            input_2_bus[0],
            output_bus[0]
        )
        signal.Multiplexer2To1(
            vcc,
            select,
            input_1_bus[1],
            input_2_bus[1],
            output_bus[1]
        )
        signal.Multiplexer2To1(
            vcc,
            select,
            input_1_bus[2],
            input_2_bus[2],
            output_bus[2]
        )
        signal.Multiplexer2To1(
            vcc,
            select,
            input_1_bus[3],
            input_2_bus[3],
            output_bus[3]
        )
        signal.Multiplexer2To1(
            vcc,
            select,
            input_1_bus[4],
            input_2_bus[4],
            output_bus[4]
        )
        signal.Multiplexer2To1(
            vcc,
            select,
            input_1_bus[5],
            input_2_bus[5],
            output_bus[5]
        )
        signal.Multiplexer2To1(
            vcc,
            select,
            input_1_bus[6],
            input_2_bus[6],
            output_bus[6]
        )
        signal.Multiplexer2To1(
            vcc,
            select,
            input_1_bus[7],
            input_2_bus[7],
            output_bus[7]
        )
        signal.Multiplexer2To1(
            vcc,
            select,
            input_1_bus[8],
            input_2_bus[8],
            output_bus[8]
        )
        signal.Multiplexer2To1(
            vcc,
            select,
            input_1_bus[9],
            input_2_bus[9],
            output_bus[9]
        )
        signal.Multiplexer2To1(
            vcc,
            select,
            input_1_bus[10],
            input_2_bus[10],
            output_bus[10]
        )
        signal.Multiplexer2To1(
            vcc,
            select,
            input_1_bus[11],
            input_2_bus[11],
            output_bus[11]
        )
        signal.Multiplexer2To1(
            vcc,
            select,
            input_1_bus[12],
            input_2_bus[12],
            output_bus[12]
        )
        signal.Multiplexer2To1(
            vcc,
            select,
            input_1_bus[13],
            input_2_bus[13],
            output_bus[13]
        )
        signal.Multiplexer2To1(
            vcc,
            select,
            input_1_bus[14],
            input_2_bus[14],
            output_bus[14]
        )
        signal.Multiplexer2To1(
            vcc,
            select,
            input_1_bus[15],
            input_2_bus[15],
            output_bus[15]
        )


class _Multiplexer8To1_16:
    """
    This is an internal module for the ALU. It multiplexes eight 16-bit inputs
    to a single 16-bit output.
    """
    def __init__(
        self,
        select_1,
        select_2,
        select_3,
        input_1_bus,
        input_2_bus,
        input_3_bus,
        input_4_bus,
        input_5_bus,
        input_6_bus,
        input_7_bus,
        input_8_bus,
        output_bus
    ):
        vcc = Wire()
        vcc.value = 1

        bus_1 = Bus8(
            input_1_bus[0],
            input_2_bus[0],
            input_3_bus[0],
            input_4_bus[0],
            input_5_bus[0],
            input_6_bus[0],
            input_7_bus[0],
            input_8_bus[0]
        )

        bus_2 = Bus8(
            input_1_bus[1],
            input_2_bus[1],
            input_3_bus[1],
            input_4_bus[1],
            input_5_bus[1],
            input_6_bus[1],
            input_7_bus[1],
            input_8_bus[1]
        )

        bus_3 = Bus8(
            input_1_bus[2],
            input_2_bus[2],
            input_3_bus[2],
            input_4_bus[2],
            input_5_bus[2],
            input_6_bus[2],
            input_7_bus[2],
            input_8_bus[2]
        )

        bus_4 = Bus8(
            input_1_bus[3],
            input_2_bus[3],
            input_3_bus[3],
            input_4_bus[3],
            input_5_bus[3],
            input_6_bus[3],
            input_7_bus[3],
            input_8_bus[3]
        )

        bus_5 = Bus8(
            input_1_bus[4],
            input_2_bus[4],
            input_3_bus[4],
            input_4_bus[4],
            input_5_bus[4],
            input_6_bus[4],
            input_7_bus[4],
            input_8_bus[4]
        )

        bus_6 = Bus8(
            input_1_bus[5],
            input_2_bus[5],
            input_3_bus[5],
            input_4_bus[5],
            input_5_bus[5],
            input_6_bus[5],
            input_7_bus[5],
            input_8_bus[5]
        )

        bus_7 = Bus8(
            input_1_bus[6],
            input_2_bus[6],
            input_3_bus[6],
            input_4_bus[6],
            input_5_bus[6],
            input_6_bus[6],
            input_7_bus[6],
            input_8_bus[6]
        )

        bus_8 = Bus8(
            input_1_bus[7],
            input_2_bus[7],
            input_3_bus[7],
            input_4_bus[7],
            input_5_bus[7],
            input_6_bus[7],
            input_7_bus[7],
            input_8_bus[7]
        )
        bus_9 = Bus8(
            input_1_bus[8],
            input_2_bus[8],
            input_3_bus[8],
            input_4_bus[8],
            input_5_bus[8],
            input_6_bus[8],
            input_7_bus[8],
            input_8_bus[8]
        )

        bus_10 = Bus8(
            input_1_bus[9],
            input_2_bus[9],
            input_3_bus[9],
            input_4_bus[9],
            input_5_bus[9],
            input_6_bus[9],
            input_7_bus[9],
            input_8_bus[9]
        )

        bus_11 = Bus8(
            input_1_bus[10],
            input_2_bus[10],
            input_3_bus[10],
            input_4_bus[10],
            input_5_bus[10],
            input_6_bus[10],
            input_7_bus[10],
            input_8_bus[10]
        )

        bus_12 = Bus8(
            input_1_bus[11],
            input_2_bus[11],
            input_3_bus[11],
            input_4_bus[11],
            input_5_bus[11],
            input_6_bus[11],
            input_7_bus[11],
            input_8_bus[11]
        )

        bus_13 = Bus8(
            input_1_bus[12],
            input_2_bus[12],
            input_3_bus[12],
            input_4_bus[12],
            input_5_bus[12],
            input_6_bus[12],
            input_7_bus[12],
            input_8_bus[12]
        )

        bus_14 = Bus8(
            input_1_bus[13],
            input_2_bus[13],
            input_3_bus[13],
            input_4_bus[13],
            input_5_bus[13],
            input_6_bus[13],
            input_7_bus[13],
            input_8_bus[13]
        )

        bus_15 = Bus8(
            input_1_bus[14],
            input_2_bus[14],
            input_3_bus[14],
            input_4_bus[14],
            input_5_bus[14],
            input_6_bus[14],
            input_7_bus[14],
            input_8_bus[14]
        )

        bus_16 = Bus8(
            input_1_bus[15],
            input_2_bus[15],
            input_3_bus[15],
            input_4_bus[15],
            input_5_bus[15],
            input_6_bus[15],
            input_7_bus[15],
            input_8_bus[15]
        )

        signal.Multiplexer8To1(
            vcc,
            select_1,
            select_2,
            select_3,
            bus_1,
            output_bus[0]
        )
        signal.Multiplexer8To1(
            vcc,
            select_1,
            select_2,
            select_3,
            bus_2,
            output_bus[1]
        )
        signal.Multiplexer8To1(
            vcc,
            select_1,
            select_2,
            select_3,
            bus_3,
            output_bus[2]
        )
        signal.Multiplexer8To1(
            vcc,
            select_1,
            select_2,
            select_3,
            bus_4,
            output_bus[3]
        )
        signal.Multiplexer8To1(
            vcc,
            select_1,
            select_2,
            select_3,
            bus_5,
            output_bus[4]
        )
        signal.Multiplexer8To1(
            vcc,
            select_1,
            select_2,
            select_3,
            bus_6,
            output_bus[5]
        )
        signal.Multiplexer8To1(
            vcc,
            select_1,
            select_2,
            select_3,
            bus_7,
            output_bus[6]
        )
        signal.Multiplexer8To1(
            vcc,
            select_1,
            select_2,
            select_3,
            bus_8,
            output_bus[7]
        )
        signal.Multiplexer8To1(
            vcc,
            select_1,
            select_2,
            select_3,
            bus_9,
            output_bus[8]
        )
        signal.Multiplexer8To1(
            vcc,
            select_1,
            select_2,
            select_3,
            bus_10,
            output_bus[9]
        )
        signal.Multiplexer8To1(
            vcc,
            select_1,
            select_2,
            select_3,
            bus_11,
            output_bus[10]
        )
        signal.Multiplexer8To1(
            vcc,
            select_1,
            select_2,
            select_3,
            bus_12,
            output_bus[11]
        )
        signal.Multiplexer8To1(
            vcc,
            select_1,
            select_2,
            select_3,
            bus_13,
            output_bus[12]
        )
        signal.Multiplexer8To1(
            vcc,
            select_1,
            select_2,
            select_3,
            bus_14,
            output_bus[13]
        )
        signal.Multiplexer8To1(
            vcc,
            select_1,
            select_2,
            select_3,
            bus_15,
            output_bus[14]
        )
        signal.Multiplexer8To1(
            vcc,
            select_1,
            select_2,
            select_3,
            bus_16,
            output_bus[15]
        )
