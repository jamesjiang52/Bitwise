"""
The following classes are defined:
    Processor
"""

from .. import wire
from .. import gate
from .. import logic
from .. import signal
from .. import state
from .. import storage
from . import ALU
from . import BUS

Wire = wire.Wire
Clock = wire.Clock
Bus4 = wire.Bus4
Bus8 = wire.Bus8
Bus10 = BUS.Bus10
Bus16 = wire.Bus16
Buffer8 = wire.BufferBus8


class Processor:
    """Construct a new 8-bit processor. It contains four 8-bit registers and
    communicates via an external 8-bit data bus.

    Operations are 10-bits wide. In cases where the number of bits that signify
    an operation is less than 10, only the most significant bits are used; the
    rest are ignored.

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
    def __init__(self, clock, instruction, instruction_available, data):
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
        alu_function_select = Bus4()
        p = Wire()
        q = Wire()

        self.datapath = _ProcessorDatapath(
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

        self.controlpath = _ProcessorControlpath(
            clock,
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

    def get_registers(self):
        return self.datapath.get_registers()

    def get_important_wires(self):
        return self.controlpath.get_important_wires()

    def __str__(self):
        return str(
            tuple([register.wire_values for register in self.get_registers()])
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
            mux_input_0_bus,
            alu_greater_than,
            alu_equal_to,
            alu_less_than
        )

        self.reg_0_out_bus = reg_0_out_bus
        self.reg_1_out_bus = reg_1_out_bus
        self.reg_2_out_bus = reg_2_out_bus
        self.reg_3_out_bus = reg_3_out_bus

    def get_registers(self):
        return (
            self.reg_0_out_bus,
            self.reg_1_out_bus,
            self.reg_2_out_bus,
            self.reg_3_out_bus
        )


class _ProcessorControlpath:
    """
    This is the internal controlpath module of the processor.
    """
    def __init__(
        self,
        clock,
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
        aclear = Wire()
        clear = Wire()
        clear_n = Wire()
        done = Wire()
        func_reg_in = Wire()
        not_w = Wire()
        and_T0__not_w = Wire()
        or_I9_I10 = Wire()
        or_I11_I12 = Wire()
        or_I1_I11_I12 = Wire()
        or_I2_I3_I4_I8 = Wire()
        or_I3_I4_I5_I6 = Wire()
        or_I2_I4_I6 = Wire()
        and_T2__or_I2_I4_I6 = Wire()
        and_T1__or_I1_I11_I12 = Wire()
        or_I2_I3_I4_I5 = Wire()
        or_I6_I8_I9_I10 = Wire()
        or_I2_to_I10 = Wire()
        or_I1_I7_I12 = Wire()
        or_I1_to_I12 = Wire()
        and_T2__or_I2_to_I10 = Wire()
        and_T3__or_I2_to_I10 = Wire()
        and_T1_I7 = Wire()
        and_T2_I7 = Wire()
        vcc = Wire(1)

        instruction_reg = Bus10()
        I_reversed = Bus16()
        I_ = Bus16(*I_reversed[::-1])
        op = Bus4(*instruction_reg[0:4])
        T_reversed = Bus4()
        T = Bus4(*T_reversed[::-1])
        A = Bus4()
        B = Bus4()
        C = Bus4()
        reg_x_in = Bus4(reg_0_in, reg_1_in, reg_2_in, reg_3_in)
        reg_x_out = Bus4(reg_0_out, reg_1_out, reg_2_out, reg_3_out)
        T1_4 = Bus4(T[1], T[1], T[1], T[1])
        T2_4 = Bus4(T[2], T[2], T[2], T[2])
        T3_4 = Bus4(T[3], T[3], T[3], T[3])
        I1_4 = Bus4(I_[1], I_[1], I_[1], I_[1])
        I7_4 = Bus4(I_[7], I_[7], I_[7], I_[7])
        I11_4 = Bus4(I_[11], I_[11], I_[11], I_[11])
        or_I2_to_I10_4 = Bus4(
            or_I2_to_I10,
            or_I2_to_I10,
            or_I2_to_I10,
            or_I2_to_I10
        )
        or_I1_to_I12_4 = Bus4(
            or_I1_to_I12,
            or_I1_to_I12,
            or_I1_to_I12,
            or_I1_to_I12
        )
        and_T1_A_I11_4 = Bus4()
        and_T1_B_I1_4 = Bus4()
        and_T2_B_I7_4 = Bus4()
        and_T3_C__or_I2_to_I10_4 = Bus4()
        and_T1_A__or_I1_to_I12_4 = Bus4()
        and_T2_B__or_I2_to_I10_4 = Bus4()

        gate.Buffer(and_T1_I7, alu_function_select[3])

        gate.NOTGate(instruction_available, not_w)
        gate.NOTGate(clear, clear_n)

        gate.ORGate3(
            and_T1__or_I1_I11_I12,
            and_T2_I7,
            and_T3__or_I2_to_I10,
            done
        )
        gate.ORGate2(and_T0__not_w, done, aclear)
        gate.ORGate2(I_[9], I_[10], or_I9_I10)
        gate.ORGate2(I_[11], I_[12], or_I11_I12)
        gate.ORGate3(I_[1], I_[11], I_[12], or_I1_I11_I12)
        gate.ORGate4(I_[2], I_[3], I_[4], I_[8], or_I2_I3_I4_I8)
        gate.ORGate4(I_[3], I_[4], I_[5], I_[6], or_I3_I4_I5_I6)
        gate.ORGate3(I_[2], I_[4], I_[6], or_I2_I4_I6)
        gate.ORGate4(I_[2], I_[3], I_[4], I_[5], or_I2_I3_I4_I5)
        gate.ORGate4(I_[6], I_[8], I_[9], I_[10], or_I6_I8_I9_I10)
        gate.ORGate2(or_I2_I3_I4_I5, or_I6_I8_I9_I10, or_I2_to_I10)
        gate.ORGate3(I_[1], I_[7], I_[12], or_I1_I7_I12)
        gate.ORGate3(
            or_I2_I3_I4_I5,
            or_I6_I8_I9_I10,
            or_I1_I7_I12,
            or_I1_to_I12
        )
        gate.ORGate2(and_T2__or_I2_I4_I6, and_T1_I7, alu_function_select[2])
        gate.ORGate2(and_T2__or_I2_to_I10, and_T1_I7, reg_alu_in)
        gate.ORGate2(and_T3__or_I2_to_I10, and_T2_I7, reg_alu_out)

        gate.ANDGate2(clock, aclear, clear)
        gate.ANDGate2(instruction_available, T[0], func_reg_in)
        gate.ANDGate2(not_w, T[0], and_T0__not_w)
        gate.ANDGate2(T[2], or_I2_I4_I6, and_T2__or_I2_I4_I6)
        gate.ANDGate2(T[2], or_I2_to_I10, and_T2__or_I2_to_I10)
        gate.ANDGate2(T[3], or_I2_to_I10, and_T3__or_I2_to_I10)
        gate.ANDGate2(T[1], or_I1_I11_I12, and_T1__or_I1_I11_I12)
        gate.ANDGate2(T[1], I_[7], and_T1_I7)
        gate.ANDGate2(T[2], I_[7], and_T2_I7)
        gate.ANDGate2(T[2], or_I9_I10, p)
        gate.ANDGate2(T[2], I_[10], q)
        gate.ANDGate2(T[1], or_I11_I12, extern)
        gate.ANDGate2(T[2], or_I2_I3_I4_I8, alu_function_select[0])
        gate.ANDGate2(T[2], or_I3_I4_I5_I6, alu_function_select[1])
        gate.ANDGate2(T[1], or_I2_to_I10, reg_temp_in)

        _BitwiseAND4_3(T1_4, A, I11_4, and_T1_A_I11_4)
        _BitwiseAND4_3(T1_4, B, I1_4, and_T1_B_I1_4)
        _BitwiseAND4_3(T2_4, B, I7_4, and_T2_B_I7_4)
        _BitwiseAND4_3(T3_4, C, or_I2_to_I10_4, and_T3_C__or_I2_to_I10_4)
        _BitwiseAND4_3(T1_4, A, or_I1_to_I12_4, and_T1_A__or_I1_to_I12_4)
        _BitwiseAND4_3(T2_4, B, or_I2_to_I10_4, and_T2_B__or_I2_to_I10_4)

        logic.BitwiseOR4(
            and_T1_A__or_I1_to_I12_4,
            and_T2_B__or_I2_to_I10_4,
            reg_x_out
        )
        _BitwiseOR4_4(
            and_T1_A_I11_4,
            and_T1_B_I1_4,
            and_T2_B_I7_4,
            and_T3_C__or_I2_to_I10_4,
            reg_x_in
        )

        _Register10(instruction, func_reg_in, clock, instruction_reg)

        signal.Decoder1Of16(vcc, op, I_reversed)
        signal.Decoder1Of4(vcc, instruction_reg[4], instruction_reg[5], A)
        signal.Decoder1Of4(vcc, instruction_reg[6], instruction_reg[7], B)
        signal.Decoder1Of4(vcc, instruction_reg[8], instruction_reg[9], C)

        state.RingCounter4(vcc, clear_n, clock, T_reversed)

        self.T = T
        self.instruction_reg = instruction_reg
        self.reg_x_out = reg_x_out
        self.clear_n = clear_n

    def get_important_wires(self):
        return self.T.wire_values, self.instruction_reg.wire_values, self.reg_x_out.wire_values, self.clear_n.value


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


class _BitwiseAND4_3:
    """This is an internal module for the processor controlpath. It performs a
    bitwise AND operation on three 4-bit inputs.
    """
    def __init__(self, a_bus, b_bus, c_bus, output_bus):
        gate.ANDGate3(a_bus[0], b_bus[0], c_bus[0], output_bus[0])
        gate.ANDGate3(a_bus[1], b_bus[1], c_bus[1], output_bus[1])
        gate.ANDGate3(a_bus[2], b_bus[2], c_bus[2], output_bus[2])
        gate.ANDGate3(a_bus[3], b_bus[3], c_bus[3], output_bus[3])


class _BitwiseOR4_4:
    """This is an internal module for the processor controlpath. It performs a
    bitwise OR operation on four 4-bit inputs.
    """
    def __init__(self, a_bus, b_bus, c_bus, d_bus, output_bus):
        gate.ORGate4(a_bus[0], b_bus[0], c_bus[0], d_bus[0], output_bus[0])
        gate.ORGate4(a_bus[1], b_bus[1], c_bus[1], d_bus[1], output_bus[1])
        gate.ORGate4(a_bus[2], b_bus[2], c_bus[2], d_bus[2], output_bus[2])
        gate.ORGate4(a_bus[3], b_bus[3], c_bus[3], d_bus[3], output_bus[3])


class _Register10:
    """This is an internal module for the processor controlpath. It stores a
    10-bit value.
    """
    def __init__(
        self,
        input_bus,
        enable,
        clock,
        output_bus,
    ):
        not_1 = Wire()
        not_2 = Wire()
        not_3 = Wire()
        not_4 = Wire()
        not_5 = Wire()
        not_6 = Wire()
        not_7 = Wire()
        not_8 = Wire()
        not_9 = Wire()
        not_10 = Wire()
        and_1 = Wire()

        gate.ANDGate2(clock, enable, and_1)

        storage.DFlipFlop(input_bus[0], and_1, output_bus[0], not_1)
        storage.DFlipFlop(input_bus[1], and_1, output_bus[1], not_2)
        storage.DFlipFlop(input_bus[2], and_1, output_bus[2], not_3)
        storage.DFlipFlop(input_bus[3], and_1, output_bus[3], not_4)
        storage.DFlipFlop(input_bus[4], and_1, output_bus[4], not_5)
        storage.DFlipFlop(input_bus[5], and_1, output_bus[5], not_6)
        storage.DFlipFlop(input_bus[6], and_1, output_bus[6], not_7)
        storage.DFlipFlop(input_bus[7], and_1, output_bus[7], not_8)
        storage.DFlipFlop(input_bus[8], and_1, output_bus[8], not_9)
        storage.DFlipFlop(input_bus[9], and_1, output_bus[9], not_10)
