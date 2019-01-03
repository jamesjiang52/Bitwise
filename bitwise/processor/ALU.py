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
    """Construct a new arithmetic-logic unit with the following functions:
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
        1110: a TIMES b (MOD 256)
        1111: NOT (a TIMES b (MOD 256))

    Args:
        a_bus: An object of type Bus8. The first input to the ALU. The first
            addend in add operations, the minuend in subtract operations, and
            the multiplicand in multiply operations. Also the number to be
            compared. a_bus[0] and a_bus[7] are the most and least significant
            bit, respectively.
        b_bus: An object of type Bus8. The second input to the ALU. The second
            addend in add operations, the subtrahend in subtract operations,
            and the multiplier in multiply operations. Also the number to be
            compared against. b_bus[0] and b_bus[7] are the most and least
            significant bit, respectively.
        function_select_bus: An object of type Bus4. The function select input
            of the ALU, with functions defined above. function_select_bus[0]
            and function_select_bus[3] are the most and least significant bit,
            respectively.
        overflow: An object of type Wire. The arithmetic overflow indicator.
            Only valid for functions 1100 and 1101 (subtract operations).
        carry_out: An object of type Wire. The carry-out. Only valid for
            functions 1010 and 1011 (add operations).
        output_bus: An object of type Bus8. The output of the ALU.
            output_bus[0] and output_bus[7] are the most and least significant
            bit, respectively.
        greater_than: An object of type Wire. The greater-than indicator.
        equal_to: An object of type Wire. The equal-to indicator.
        less_than: An object of type Wire. The less-than indicator.

    Raises:
        TypeError: If either a_bus, b_bus, or output_bus is not a bus of width
            8, or if function_select_bus is not a bus of width 4.
    """
    def __init__(
        self,
        a_bus,
        b_bus,
        function_select_bus,
        overflow,
        carry_out,
        output_bus,
        greater_than,
        equal_to,
        less_than
    ):
        if len(a_bus) != 8:
            raise TypeError(
                "Expected bus of width 8, received bus of width {0}.".format(
                    len(a_bus)
                )
            )

        if len(b_bus) != 8:
            raise TypeError(
                "Expected bus of width 8, received bus of width {0}.".format(
                    len(b_bus)
                )
            )

        if len(output_bus) != 8:
            raise TypeError(
                "Expected bus of width 8, received bus of width {0}.".format(
                    len(output_bus)
                )
            )

        if len(function_select_bus) != 4:
            raise TypeError(
                "Expected bus of width 4, received bus of width {0}.".format(
                    len(function_select_bus)
                )
            )

        a_and_b_bus = Bus8()
        a_or_b_bus = Bus8()
        a_xor_b_bus = Bus8()
        a_plus_b_bus = Bus8()
        a_times_b_bus_16 = Bus16()
        a_times_b_bus = Bus8(*a_times_b_bus_16[8:16])

        true_bus = Bus8()
        not_bus = Bus8()

        _Multiplexer8To1_8(
            function_select_bus[0],
            function_select_bus[1],
            function_select_bus[2],
            a_times_b_bus,
            a_plus_b_bus,
            a_plus_b_bus,
            a_xor_b_bus,
            a_or_b_bus,
            a_and_b_bus,
            b_bus,
            a_bus,
            true_bus
        )

        logic.BitwiseAND8(a_bus, b_bus, a_and_b_bus)
        logic.BitwiseOR8(a_bus, b_bus, a_or_b_bus)
        logic.BitwiseXOR8(a_bus, b_bus, a_xor_b_bus)
        arithmetic.AdderSubtractor8(
            function_select_bus[1],
            a_bus,
            b_bus,
            overflow,
            carry_out,
            a_plus_b_bus
        )
        arithmetic.Multiplier8(a_bus, b_bus, a_times_b_bus_16)

        logic.BitwiseNOT8(true_bus, not_bus)

        _Multiplexer2To1_8(
            function_select_bus[3],
            not_bus,
            true_bus,
            output_bus
        )

        logic.Comparator7(a_bus, b_bus, greater_than, equal_to, less_than)


class _Multiplexer2To1_8:
    """
    This is an internal module for the ALU. It multiplexes two 8-bit inputs to
    a single 8-bit output.
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


class _Multiplexer8To1_8:
    """
    This is an internal module for the ALU. It multiplexes eight 8-bit inputs
    to a single 8-bit output.
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

        bus_2 = Bus16(
            input_1_bus[1],
            input_2_bus[1],
            input_3_bus[1],
            input_4_bus[1],
            input_5_bus[1],
            input_6_bus[1],
            input_7_bus[1],
            input_8_bus[1]
        )

        bus_3 = Bus16(
            input_1_bus[2],
            input_2_bus[2],
            input_3_bus[2],
            input_4_bus[2],
            input_5_bus[2],
            input_6_bus[2],
            input_7_bus[2],
            input_8_bus[2]
        )

        bus_4 = Bus16(
            input_1_bus[3],
            input_2_bus[3],
            input_3_bus[3],
            input_4_bus[3],
            input_5_bus[3],
            input_6_bus[3],
            input_7_bus[3],
            input_8_bus[3]
        )

        bus_5 = Bus16(
            input_1_bus[4],
            input_2_bus[4],
            input_3_bus[4],
            input_4_bus[4],
            input_5_bus[4],
            input_6_bus[4],
            input_7_bus[4],
            input_8_bus[4]
        )

        bus_6 = Bus16(
            input_1_bus[5],
            input_2_bus[5],
            input_3_bus[5],
            input_4_bus[5],
            input_5_bus[5],
            input_6_bus[5],
            input_7_bus[5],
            input_8_bus[5]
        )

        bus_7 = Bus16(
            input_1_bus[6],
            input_2_bus[6],
            input_3_bus[6],
            input_4_bus[6],
            input_5_bus[6],
            input_6_bus[6],
            input_7_bus[6],
            input_8_bus[6]
        )

        bus_8 = Bus16(
            input_1_bus[7],
            input_2_bus[7],
            input_3_bus[7],
            input_4_bus[7],
            input_5_bus[7],
            input_6_bus[7],
            input_7_bus[7],
            input_8_bus[7]
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
