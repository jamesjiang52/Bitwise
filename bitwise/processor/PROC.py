"""
The following classes are defined:
"""

from .. import wire
from .. import storage
from . import ALU

Wire = wire.Wire
Bus8 = wire.Bus8
Buffer8 = wire.BufferBus8


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
        alu_function_select
    ):

        alu_greater_than = Wire()
        alu_equal_to = Wire()
        alu_less_than = Wire()
        alu_overflow = Wire()
        alu_carry_out = Wire()

        d1 = Wire()
        d2 = Wire()
        d3 = Wire()
        d4 = Wire()
        d5 = Wire()
        d6 = Wire()
        d7 = Wire()
        d8 = Wire()
        data_bus = Bus8(d1, d2, d3, d4, d5, d6, d7, d8)

        r01 = Wire()
        r02 = Wire()
        r03 = Wire()
        r04 = Wire()
        r05 = Wire()
        r06 = Wire()
        r07 = Wire()
        r08 = Wire()
        reg_0_out_bus = Bus8(r01, r02, r03, r04, r05, r06, r07, r08)

        r11 = Wire()
        r12 = Wire()
        r13 = Wire()
        r14 = Wire()
        r15 = Wire()
        r16 = Wire()
        r17 = Wire()
        r18 = Wire()
        reg_1_out_bus = Bus8(r11, r12, r13, r14, r15, r16, r17, r18)

        r21 = Wire()
        r22 = Wire()
        r23 = Wire()
        r24 = Wire()
        r25 = Wire()
        r26 = Wire()
        r27 = Wire()
        r28 = Wire()
        reg_2_out_bus = Bus8(r21, r22, r23, r24, r25, r26, r27, r28)

        r31 = Wire()
        r32 = Wire()
        r33 = Wire()
        r34 = Wire()
        r35 = Wire()
        r36 = Wire()
        r37 = Wire()
        r38 = Wire()
        reg_3_out_bus = Bus8(r31, r32, r33, r34, r35, r36, r37, r38)

        tmp1 = Wire()
        tmp2 = Wire()
        tmp3 = Wire()
        tmp4 = Wire()
        tmp5 = Wire()
        tmp6 = Wire()
        tmp7 = Wire()
        tmp8 = Wire()
        reg_temp_out_bus = Bus8(tmp1, tmp2, tmp3, tmp4, tmp5, tmp6, tmp7, tmp8)

        alu1 = Wire()
        alu2 = Wire()
        alu3 = Wire()
        alu4 = Wire()
        alu5 = Wire()
        alu6 = Wire()
        alu7 = Wire()
        alu8 = Wire()
        alu_out_bus = Bus8(alu1, alu2, alu3, alu4, alu5, alu6, alu7, alu8)

        o1 = Wire()
        o2 = Wire()
        o3 = Wire()
        o4 = Wire()
        o5 = Wire()
        o6 = Wire()
        o7 = Wire()
        o8 = Wire()
        reg_alu_out_bus = Bus8(o1, o2, o3, o4, o5, o6, o7, o8)

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

        ALU.ArithmeticLogicUnit(
            reg_temp_out_bus,
            data_bus,
            alu_function_select,
            alu_overflow,
            alu_carry_out,
            alu_out_bus,
            alu_greater_than,
            alu_equal_to,
            alu_less_than
        )
