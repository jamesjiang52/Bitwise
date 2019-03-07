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
from . import SP
from . import PC
from . import FLAG

Wire = wire.Wire
Clock = wire.Clock
Bus4 = wire.Bus4
Bus8 = wire.Bus8
Bus16 = wire.Bus16
BufferBus4 = wire.BufferBus4
BufferBus8 = wire.BufferBus8
BufferBus16 = wire.BufferBus16


class Processor:
    """Construct a new 16-bit processor. It contains sixteen 16-bit registers
    and communicates via two external 16-bit data buses and an address bus.

    Instructions are 16-bits wide. In cases where the number of bits required
    to represent the instruction is less than 16, only the most significant
    bits are used; the rest are ignored.

    The following is a list of op codes, where a, b, and c are 4-bit values
    representing registers:
        0000: Halt execution. Internally, this is done by decrementing the
            program counter.
        0001 a b: Copy the value of register b into register a.
        0010 a #b: Store the immediate value of b in register a. b is taken as
            the value in the memory address immediately proceeding the store
            instruction.
        0011 a b c: Add the value of register b and the value of register c and
            store the result in register a.
        0100 a b c: Subtract the value of register c from the value of register
            b and store the result in register a.
        0101 a b c: Bitwise AND the value of register b and the value of
            register c and store the result in register a.
        0110 a b c: Bitwise OR the value of register b and the value of
            register c and store the result in register a.
        0111 a b c: Bitwise XOR the value of register b and the value of
            register c and store the result in register a.
        1000 a b: Bitwise NOT the value of register b and store the result in
            register a.
        1001 a: Branch to the memory location given in register a.
        1010 a b c: Set the value of register a to 1 if the value of register b
            is greater than the value of register c. Otherwise, set register a
            to 0.
        1011 a b c: Set the value of register a to 1 if the value of register b
            is equal to the value of register c. Otherwise, set register a to
            0.
        1100 a [b]: Load the contents of the memory location given in register
            b into register a.
        1101 a [b]: Store the contents of register a in the memory location
            given by register b.
        1110 a: Push the value of register a onto the stack. A stack overflow
            results in undefined behavior.
        1111 a: Pop the top value off the stack and store it in register a. An
            empty stack results in undefined behavior.
    """


class _ProcessorDatapath:
    """
    """
    def __init__(
        self,
        clock,
        data_ext,
        reg_in,
        reg_out,
        up_sp,
        down_sp,
        up_pc,
        load_pc,
        a_in,
        flags_in,
        g_in,
        g_out,
        data_in,
        addr_in,
        sel_bus,
        ext,
        ir_in,
        instruction,
        z_f,
        v_f,
        n_f,
        c_f,
        data,
        addr
    ):
        v = Wire()
        c = Wire()
        data_bus = Bus16()
        r0_out = Bus16()
        r1_out = Bus16()
        r2_out = Bus16()
        r3_out = Bus16()
        r4_out = Bus16()
        r5_out = Bus16()
        r6_out = Bus16()
        r7_out = Bus16()
        r8_out = Bus16()
        r9_out = Bus16()
        r10_out = Bus16()
        r11_out = Bus16()
        r12_out = Bus16()
        r13_out = Bus16()
        sp_out = Bus16()
        pc_out = Bus16()
        a_out = Bus16()
        alu_out = Bus16()
        gr_out = Bus16()

        # register array
        storage.Register16(data_bus, reg_in[0], clock, r0_out)
        storage.Register16(data_bus, reg_in[1], clock, r1_out)
        storage.Register16(data_bus, reg_in[2], clock, r2_out)
        storage.Register16(data_bus, reg_in[3], clock, r3_out)
        storage.Register16(data_bus, reg_in[4], clock, r4_out)
        storage.Register16(data_bus, reg_in[5], clock, r5_out)
        storage.Register16(data_bus, reg_in[6], clock, r6_out)
        storage.Register16(data_bus, reg_in[7], clock, r7_out)
        storage.Register16(data_bus, reg_in[8], clock, r8_out)
        storage.Register16(data_bus, reg_in[9], clock, r9_out)
        storage.Register16(data_bus, reg_in[10], clock, r10_out)
        storage.Register16(data_bus, reg_in[11], clock, r11_out)
        storage.Register16(data_bus, reg_in[12], clock, r12_out)
        storage.Register16(data_bus, reg_in[13], clock, r13_out)
        SP.StackPointer(up_sp, down_sp, clock, sp_out)
        PC.ProgramCounter(data_bus, up_pc, load_pc, clock, pc_out)

        BufferBus16(reg_out[0], r0_out, data_bus)
        BufferBus16(reg_out[1], r1_out, data_bus)
        BufferBus16(reg_out[2], r2_out, data_bus)
        BufferBus16(reg_out[3], r3_out, data_bus)
        BufferBus16(reg_out[4], r4_out, data_bus)
        BufferBus16(reg_out[5], r5_out, data_bus)
        BufferBus16(reg_out[6], r6_out, data_bus)
        BufferBus16(reg_out[7], r7_out, data_bus)
        BufferBus16(reg_out[8], r8_out, data_bus)
        BufferBus16(reg_out[9], r9_out, data_bus)
        BufferBus16(reg_out[10], r10_out, data_bus)
        BufferBus16(reg_out[11], r11_out, data_bus)
        BufferBus16(reg_out[12], r12_out, data_bus)
        BufferBus16(reg_out[13], r13_out, data_bus)
        BufferBus16(reg_out[14], sp_out, data_bus)
        BufferBus16(reg_out[15], pc_out, data_bus)

        # arithmetic-logic unit
        storage.Register16(data_bus, a_in, clock, a_out)
        ALU.ArithmeticLogicUnit(a_out, data_bus, sel_bus, v, c, alu_out)
        FLAG.ConditionCodeFlags(data_bus, v, c, clock, z_f, v_f, n_f, c_f)
        storage.Register16(alu_out, g_in, clock, gr_out)
        BufferBus16(g_out, gr_out, data_bus)

        # external
        storage.Register16(data_bus, data_in, clock, data)
        storage.Register16(data_bus, addr_in, clock, addr)
        BufferBus16(ext, data_ext, data_bus)

        # instruction register
        storage.Register16(data_bus, ir_in, clock, instruction)


class _ProcessorControlpath:
    """
    """
    def __init__(
        self,
        instruction,
        z,
        v,
        n,
        c,
        reg_in,
        reg_out,
        up_sp,
        down_sp,
        up_pc,
        load_pc,
        a_in,
        flags_in,
        g_in,
        g_out,
        data_in,
        addr_in,
        sel_bus,
        ext,
        ir_in,
        write_enable
    ):
        vcc = Wire(1)
        op = Bus4(*instruction[0:4])
        a = Bus4(*instruction[4:8])
        b = Bus4(*instruction[8:12])
        c = Bus4(*instruction[12:16])

        op_dec = Bus16()
        a_dec = Bus16()
        b_dec = Bus16()
        c_dec = Bus16()

        signal.Decoder1Of16(vcc, op, op_dec)
        signal.Decoder1Of16(vcc, a, a_dec)
        signal.Decoder1Of16(vcc, b, b_dec)
        signal.Decoder1Of16(vcc, c, c_dec)
