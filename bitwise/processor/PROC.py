"""
The following classes are defined:
"""

from .. import wire
from .. import storage
from . import ALU

Wire = wire.Wire
Bus8 = wire.Bus8
Buffer8 = wire.BufferBus8


class Processor:
    """
    """
    def __init__(self):
        clock = Wire()
        data = Bus8()
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
        alu_greater_than = Wire()
        alu_equal_to = Wire()
        alu_less_than = Wire()

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
            alu_greater_than,
            alu_equal_to,
            alu_less_than
        )

        _ProcessorControlpath(
            instruction,
            instruction_available,
            alu_greater_than,
            alu_equal_to,
            alu_less_than,
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
        alu_greater_than,
        alu_equal_to,
        alu_less_than
    ):
        if len(alu_function_select) != 4:
            raise TypeError(
                "Expected bus of width 4, received bus of width {0}.".format(
                    len(alu_function_select)
                )
            )

        alu_overflow = Wire()
        alu_carry_out = Wire()

        data_bus = Bus8()

        reg_0_out_bus = Bus8()
        reg_1_out_bus = Bus8()
        reg_2_out_bus = Bus8()
        reg_3_out_bus = Bus8()

        reg_temp_out_bus = Bus8()
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


class _ProcessorControlpath:
    """
    This is the internal controlpath module of the processor.
    """
    def __init__(
        self,
        instruction,
        instruction_available,
        alu_greater_than,
        alu_equal_to,
        alu_less_than,
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
