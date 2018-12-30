"""
The following classes are defined:
"""

from .. import wire
from .. import signal
from .. import storage
from . import ALU

Wire = wire.Wire
Bus8 = wire.Bus8
Buffer8 = wire.BufferBus8


class Processor:
    """Construct a new 8-bit processor. It contains four 8-bit registers and
    communicates via an external 8-bit data bus.

    Operations are 10-bits wide. In cases where the number of bits that signify
    an operation is less than 10, only the most significant bits are used, with
    the rest ignored.

    The following is a list of op codes, where a, b, and c are 2-bit values
    representing registers:
        0000: No operation.
        0001 a b: Copy the value of register a into register b.
        0010 a b c: Add the value of register a and the value of register b and
            put the result into register c.
        0011 a b c: Subtract the value of register b from the value of register
            a and put the result into register c.
        0100 a b c: Multiply the value of register a and the value of register
            b and put the result (mod 256) into register c.
        0101 a b c: Bitwise AND the value of register a and the value of
            register b and put the result into register c.
        0110 a b c: Bitwise OR the value of register a and the value of
            register b and put the result into register c.
        0111 a b: Bitwise NOT the value of register a and put the result into
            register b.
        1000 a b c: Bitwise XOR the value of register a and the value of
            register b and put the result into register c.
        1001 a b c: Set the value of register c to 1 if the value of register a
            is greater than the value of register b. Otherwise, set register c
            to 0.
        1010 a b c: Set the value of register c to 1 if the value of register a
            is equal to the value of register b. Otherwise, set register c to
            0.
        1011 a: Load the value of the external data bus into register a.
        1100 a: Put the value of register a onto the external data bus.
        1101: No operation.
        1110: No operation.
        1111: No operation.

    For example:
        0001100111 copies the value of register 2 into register 1.
        0100010100 computes the square of the value of register 1 and stores it
            in register 0.
        1001000110 sets the value of register 2 to 1 if the value of register 0
            is greater than the value of register 1. Otherwise, sets register 2
            to 0.
        1010101011 sets the value of register 3 to 1.
        1011011101 loads the value of the external data bus into register 1.
        1100000111 puts the value of register 0 onto the external data bus.
        Both 0000000000 and 0000011101 do nothing.
        1111111111 does nothing.
    """
    def __init__(self, instruction, instruction_available, data):
        if len(instruction) != 10:
            raise TypeError(
                "Expected bus of width 10, received bus of width {0}.".format(
                    len(instruction)
                )
            )

        if len(data) != 8:
            raise TypeError(
                "Expected bus of width 8, received bus of width {0}.".format(
                    len(data)
                )
            )

        clock = Wire()
        extern = Wire()
        reg_0_in = Wire()
        reg_0_out = Wire()
        reg_1_in = Wire()
        reg_1_out = Wire()
        reg_2_in = Wire()
        reg_2_out = Wire()
        reg_3_in = Wire()
        reg_3_out = Wire()
        reg_temp_in = Wire()
        reg_alu_in = Wire()
        reg_alu_out = Wire()
        alu_function_select = Wire()
        p = Wire()
        q = Wire()

        _ProcessorDatapath(
            clock,
            data,
            extern,
            reg_0_in,
            reg_0_out,
            reg_1_in,
            reg_1_out,
            reg_2_in,
            reg_2_out,
            reg_3_in,
            reg_3_out,
            reg_temp_in,
            reg_alu_in,
            reg_alu_out,
            alu_function_select,
            p,
            q
        )

        _ProcessorControlpath(
            instruction,
            instruction_available,
            extern,
            reg_0_in,
            reg_0_out,
            reg_1_in,
            reg_1_out,
            reg_2_in,
            reg_2_out,
            reg_3_in,
            reg_3_out,
            reg_temp_in,
            reg_alu_in,
            reg_alu_out,
            alu_function_select,
            p,
            q
        )


class _ProcessorDatapath:
    """
    This is the internal datapath module of the processor.
    """
    def __init__(
        self,
        clock,
        data,
        extern,
        reg_0_in,
        reg_0_out,
        reg_1_in,
        reg_1_out,
        reg_2_in,
        reg_2_out,
        reg_3_in,
        reg_3_out,
        reg_temp_in,
        reg_alu_in,
        reg_alu_out,
        alu_function_select,
        p,
        q
    ):
        if len(data) != 4:
            raise TypeError(
                "Expected bus of width 4, received bus of width {0}.".format(
                    len(data)
                )
            )

        if len(alu_function_select) != 4:
            raise TypeError(
                "Expected bus of width 4, received bus of width {0}.".format(
                    len(alu_function_select)
                )
            )

        vcc = Wire(1)

        alu_overflow = Wire()
        alu_carry_out = Wire()
        alu_greater_than = Wire()
        alu_equal_to = Wire()
        alu_less_than = Wire()
        mux_out = Wire()

        data_bus = Bus8()

        reg_0_out_bus = Bus8()
        reg_1_out_bus = Bus8()
        reg_2_out_bus = Bus8()
        reg_3_out_bus = Bus8()

        reg_temp_out_bus = Bus8()
        mux_input_0_bus = Bus8()
        mux_input_1_bus = Bus8(wire_8=mux_out)
        alu_out_bus = Bus8()
        reg_alu_out_bus = Bus8()

        storage.Register8(data_bus, reg_0_in, clock, reg_0_out_bus)
        storage.Register8(data_bus, reg_1_in, clock, reg_1_out_bus)
        storage.Register8(data_bus, reg_2_in, clock, reg_2_out_bus)
        storage.Register8(data_bus, reg_3_in, clock, reg_3_out_bus)
        storage.Register8(data_bus, reg_temp_in, clock, reg_temp_out_bus)
        storage.Register8(alu_out_bus, reg_alu_in, clock, reg_alu_out_bus)

        Buffer8(extern, data, data_bus)
        Buffer8(reg_0_out, reg_0_out_bus, data_bus)
        Buffer8(reg_1_out, reg_1_out_bus, data_bus)
        Buffer8(reg_2_out, reg_2_out_bus, data_bus)
        Buffer8(reg_3_out, reg_3_out_bus, data_bus)
        Buffer8(reg_alu_out, reg_alu_out_bus, data_bus)

        signal.Multiplexer2To1(vcc, q, alu_equal_to, alu_greater_than, mux_out)
        _Multiplexer2To1_8(p, mux_input_1_bus, mux_input_0_bus, alu_out_bus)

        ALU.ArithmeticLogicUnit(
            reg_temp_out_bus,
            data_bus,
            alu_function_select,
            alu_overflow,
            alu_carry_out,
            alu_mux_input_0_bus,
            alu_greater_than,
            alu_equal_to,
            alu_less_than
        )


class _ProcessorControlpath:
    """
    This is the internal controlpath module of the processor.
    """
    def __init__(
        self,
        instruction,
        instruction_available,
        extern,
        reg_0_in,
        reg_0_out,
        reg_1_in,
        reg_1_out,
        reg_2_in,
        reg_2_out,
        reg_3_in,
        reg_3_out,
        reg_temp_in,
        reg_alu_in,
        reg_alu_out,
        alu_function_select,
        p,
        q
    ):


class _Multiplexer2To1_8:
    """
    This is an internal module for the processor datapath. It multiplexes two
    8-bit inputs to a single 8-bit output.
    """
    def __init__(
        self,
        select,
        input_1_bus,
        input_2_bus,
        output_bus
    ):
        if len(input_1_bus) != 8:
            raise TypeError(
                "Expected bus of width 8, received bus of width {0}.".format(
                    len(input_1_bus)
                )
            )

        if len(input_2_bus) != 8:
            raise TypeError(
                "Expected bus of width 8, received bus of width {0}.".format(
                    len(input_2_bus)
                )
            )

        if len(output_bus) != 8:
            raise TypeError(
                "Expected bus of width 8, received bus of width {0}.".format(
                    len(output_bus)
                )
            )

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
