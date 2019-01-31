"""
The following classes are defined:
    RAM16x4
    RAM256x4
    RAM65536x4
    RAM16x8
    RAM256x8
    RAM65536x8
    RAM16x16
    RAM256x16
    RAM65536x16
"""

from .. import wire
from .. import logic
from .. import signal
from . import REG

Wire = wire.Wire
Bus4 = wire.Bus4
Bus8 = wire.Bus8
Bus16 = wire.Bus16


class RAM16x4:
    """Construct a new 16-word deep 4-bit wide random access memory array.
    """
    def __init__(self, data_bus, address_bus, write_enable, clock, output_bus):
        if len(data_bus) != 4:
            raise TypeError(
                "Expected bus of width 4, received bus of width {0}.".format(
                    len(data_bus)
                )
            )

        if len(address_bus) != 4:
            raise TypeError(
                "Expected bus of width 4, received bus of width {0}.".format(
                    len(address_bus)
                )
            )

        if len(output_bus) != 4:
            raise TypeError(
                "Expected bus of width 4, received bus of width {0}.".format(
                    len(output_bus)
                )
            )

        vcc = Wire(1)
        dec_out = Bus16()
        wren = Bus16()
        wren_in = Bus16(
            write_enable,
            write_enable,
            write_enable,
            write_enable,
            write_enable,
            write_enable,
            write_enable,
            write_enable,
            write_enable,
            write_enable,
            write_enable,
            write_enable,
            write_enable,
            write_enable,
            write_enable,
            write_enable
        )
        r0 = Bus4()
        r1 = Bus4()
        r2 = Bus4()
        r3 = Bus4()
        r4 = Bus4()
        r5 = Bus4()
        r6 = Bus4()
        r7 = Bus4()
        r8 = Bus4()
        r9 = Bus4()
        r10 = Bus4()
        r11 = Bus4()
        r12 = Bus4()
        r13 = Bus4()
        r14 = Bus4()
        r15 = Bus4()

        signal.Decoder1Of16(vcc, address_bus, dec_out)
        logic.BitwiseAND16(dec_out, wren_in, wren)

        REG.Register4(data_bus, wren[0], clock, r0)
        REG.Register4(data_bus, wren[1], clock, r1)
        REG.Register4(data_bus, wren[2], clock, r2)
        REG.Register4(data_bus, wren[3], clock, r3)
        REG.Register4(data_bus, wren[4], clock, r4)
        REG.Register4(data_bus, wren[5], clock, r5)
        REG.Register4(data_bus, wren[6], clock, r6)
        REG.Register4(data_bus, wren[7], clock, r7)
        REG.Register4(data_bus, wren[8], clock, r8)
        REG.Register4(data_bus, wren[9], clock, r9)
        REG.Register4(data_bus, wren[10], clock, r10)
        REG.Register4(data_bus, wren[11], clock, r11)
        REG.Register4(data_bus, wren[12], clock, r12)
        REG.Register4(data_bus, wren[13], clock, r13)
        REG.Register4(data_bus, wren[14], clock, r14)
        REG.Register4(data_bus, wren[15], clock, r15)

        wire.BufferBus4(dec_out[0], r0, output_bus)
        wire.BufferBus4(dec_out[1], r1, output_bus)
        wire.BufferBus4(dec_out[2], r2, output_bus)
        wire.BufferBus4(dec_out[3], r3, output_bus)
        wire.BufferBus4(dec_out[4], r4, output_bus)
        wire.BufferBus4(dec_out[5], r5, output_bus)
        wire.BufferBus4(dec_out[6], r6, output_bus)
        wire.BufferBus4(dec_out[7], r7, output_bus)
        wire.BufferBus4(dec_out[8], r8, output_bus)
        wire.BufferBus4(dec_out[9], r9, output_bus)
        wire.BufferBus4(dec_out[10], r10, output_bus)
        wire.BufferBus4(dec_out[11], r11, output_bus)
        wire.BufferBus4(dec_out[12], r12, output_bus)
        wire.BufferBus4(dec_out[13], r13, output_bus)
        wire.BufferBus4(dec_out[14], r14, output_bus)
        wire.BufferBus4(dec_out[15], r15, output_bus)


class RAM256x4:
    """Construct a new 256-word deep 4-bit wide random access memory array.
    """
    def __init__(self, data_bus, address_bus, write_enable, clock, output_bus):
        if len(data_bus) != 4:
            raise TypeError(
                "Expected bus of width 4, received bus of width {0}.".format(
                    len(data_bus)
                )
            )

        if len(address_bus) != 8:
            raise TypeError(
                "Expected bus of width 8, received bus of width {0}.".format(
                    len(address_bus)
                )
            )

        if len(output_bus) != 4:
            raise TypeError(
                "Expected bus of width 4, received bus of width {0}.".format(
                    len(output_bus)
                )
            )


class RAM65536x4:
    """Construct a new 65536-word deep 4-bit wide random access memory array.
    """
    def __init__(self, data_bus, address_bus, write_enable, clock, output_bus):
        if len(data_bus) != 4:
            raise TypeError(
                "Expected bus of width 4, received bus of width {0}.".format(
                    len(data_bus)
                )
            )

        if len(address_bus) != 16:
            raise TypeError(
                "Expected bus of width 16, received bus of width {0}.".format(
                    len(address_bus)
                )
            )

        if len(output_bus) != 4:
            raise TypeError(
                "Expected bus of width 4, received bus of width {0}.".format(
                    len(output_bus)
                )
            )


class RAM16x8:
    """Construct a new 16-word deep 8-bit wide random access memory array.
    """
    def __init__(self, data_bus, address_bus, write_enable, clock, output_bus):
        if len(data_bus) != 8:
            raise TypeError(
                "Expected bus of width 8, received bus of width {0}.".format(
                    len(data_bus)
                )
            )

        if len(address_bus) != 4:
            raise TypeError(
                "Expected bus of width 4, received bus of width {0}.".format(
                    len(address_bus)
                )
            )

        if len(output_bus) != 8:
            raise TypeError(
                "Expected bus of width 8, received bus of width {0}.".format(
                    len(output_bus)
                )
            )

        vcc = Wire(1)
        dec_out = Bus16()
        wren = Bus16()
        wren_in = Bus16(
            write_enable,
            write_enable,
            write_enable,
            write_enable,
            write_enable,
            write_enable,
            write_enable,
            write_enable,
            write_enable,
            write_enable,
            write_enable,
            write_enable,
            write_enable,
            write_enable,
            write_enable,
            write_enable
        )
        r0 = Bus8()
        r1 = Bus8()
        r2 = Bus8()
        r3 = Bus8()
        r4 = Bus8()
        r5 = Bus8()
        r6 = Bus8()
        r7 = Bus8()
        r8 = Bus8()
        r9 = Bus8()
        r10 = Bus8()
        r11 = Bus8()
        r12 = Bus8()
        r13 = Bus8()
        r14 = Bus8()
        r15 = Bus8()

        signal.Decoder1Of16(vcc, address_bus, dec_out)
        logic.BitwiseAND16(dec_out, wren_in, wren)

        REG.Register8(data_bus, wren[0], clock, r0)
        REG.Register8(data_bus, wren[1], clock, r1)
        REG.Register8(data_bus, wren[2], clock, r2)
        REG.Register8(data_bus, wren[3], clock, r3)
        REG.Register8(data_bus, wren[4], clock, r4)
        REG.Register8(data_bus, wren[5], clock, r5)
        REG.Register8(data_bus, wren[6], clock, r6)
        REG.Register8(data_bus, wren[7], clock, r7)
        REG.Register8(data_bus, wren[8], clock, r8)
        REG.Register8(data_bus, wren[9], clock, r9)
        REG.Register8(data_bus, wren[10], clock, r10)
        REG.Register8(data_bus, wren[11], clock, r11)
        REG.Register8(data_bus, wren[12], clock, r12)
        REG.Register8(data_bus, wren[13], clock, r13)
        REG.Register8(data_bus, wren[14], clock, r14)
        REG.Register8(data_bus, wren[15], clock, r15)

        wire.BufferBus8(dec_out[0], r0, output_bus)
        wire.BufferBus8(dec_out[1], r1, output_bus)
        wire.BufferBus8(dec_out[2], r2, output_bus)
        wire.BufferBus8(dec_out[3], r3, output_bus)
        wire.BufferBus8(dec_out[4], r4, output_bus)
        wire.BufferBus8(dec_out[5], r5, output_bus)
        wire.BufferBus8(dec_out[6], r6, output_bus)
        wire.BufferBus8(dec_out[7], r7, output_bus)
        wire.BufferBus8(dec_out[8], r8, output_bus)
        wire.BufferBus8(dec_out[9], r9, output_bus)
        wire.BufferBus8(dec_out[10], r10, output_bus)
        wire.BufferBus8(dec_out[11], r11, output_bus)
        wire.BufferBus8(dec_out[12], r12, output_bus)
        wire.BufferBus8(dec_out[13], r13, output_bus)
        wire.BufferBus8(dec_out[14], r14, output_bus)
        wire.BufferBus8(dec_out[15], r15, output_bus)


class RAM256x8:
    """Construct a new 256-word deep 8-bit wide random access memory array.
    """
    def __init__(self, data_bus, address_bus, write_enable, clock, output_bus):
        if len(data_bus) != 8:
            raise TypeError(
                "Expected bus of width 8, received bus of width {0}.".format(
                    len(data_bus)
                )
            )

        if len(address_bus) != 8:
            raise TypeError(
                "Expected bus of width 8, received bus of width {0}.".format(
                    len(address_bus)
                )
            )

        if len(output_bus) != 8:
            raise TypeError(
                "Expected bus of width 8, received bus of width {0}.".format(
                    len(output_bus)
                )
            )


class RAM65536x8:
    """Construct a new 65536-word deep 8-bit wide random access memory array.
    """
    def __init__(self, data_bus, address_bus, write_enable, clock, output_bus):
        if len(data_bus) != 8:
            raise TypeError(
                "Expected bus of width 8, received bus of width {0}.".format(
                    len(data_bus)
                )
            )

        if len(address_bus) != 16:
            raise TypeError(
                "Expected bus of width 16, received bus of width {0}.".format(
                    len(address_bus)
                )
            )

        if len(output_bus) != 8:
            raise TypeError(
                "Expected bus of width 8, received bus of width {0}.".format(
                    len(output_bus)
                )
            )


class RAM16x16:
    """Construct a new 16-word deep 16-bit wide random access memory array.
    """
    def __init__(self, data_bus, address_bus, write_enable, clock, output_bus):
        if len(data_bus) != 16:
            raise TypeError(
                "Expected bus of width 16, received bus of width {0}.".format(
                    len(data_bus)
                )
            )

        if len(address_bus) != 4:
            raise TypeError(
                "Expected bus of width 4, received bus of width {0}.".format(
                    len(address_bus)
                )
            )

        if len(output_bus) != 16:
            raise TypeError(
                "Expected bus of width 16, received bus of width {0}.".format(
                    len(output_bus)
                )
            )

        vcc = Wire(1)
        dec_out = Bus16()
        wren = Bus16()
        wren_in = Bus16(
            write_enable,
            write_enable,
            write_enable,
            write_enable,
            write_enable,
            write_enable,
            write_enable,
            write_enable,
            write_enable,
            write_enable,
            write_enable,
            write_enable,
            write_enable,
            write_enable,
            write_enable,
            write_enable
        )
        r0 = Bus16()
        r1 = Bus16()
        r2 = Bus16()
        r3 = Bus16()
        r4 = Bus16()
        r5 = Bus16()
        r6 = Bus16()
        r7 = Bus16()
        r8 = Bus16()
        r9 = Bus16()
        r10 = Bus16()
        r11 = Bus16()
        r12 = Bus16()
        r13 = Bus16()
        r14 = Bus16()
        r15 = Bus16()

        signal.Decoder1Of16(vcc, address_bus, dec_out)
        logic.BitwiseAND16(dec_out, wren_in, wren)

        REG.Register16(data_bus, wren[0], clock, r0)
        REG.Register16(data_bus, wren[1], clock, r1)
        REG.Register16(data_bus, wren[2], clock, r2)
        REG.Register16(data_bus, wren[3], clock, r3)
        REG.Register16(data_bus, wren[4], clock, r4)
        REG.Register16(data_bus, wren[5], clock, r5)
        REG.Register16(data_bus, wren[6], clock, r6)
        REG.Register16(data_bus, wren[7], clock, r7)
        REG.Register16(data_bus, wren[8], clock, r8)
        REG.Register16(data_bus, wren[9], clock, r9)
        REG.Register16(data_bus, wren[10], clock, r10)
        REG.Register16(data_bus, wren[11], clock, r11)
        REG.Register16(data_bus, wren[12], clock, r12)
        REG.Register16(data_bus, wren[13], clock, r13)
        REG.Register16(data_bus, wren[14], clock, r14)
        REG.Register16(data_bus, wren[15], clock, r15)

        wire.BufferBus16(dec_out[0], r0, output_bus)
        wire.BufferBus16(dec_out[1], r1, output_bus)
        wire.BufferBus16(dec_out[2], r2, output_bus)
        wire.BufferBus16(dec_out[3], r3, output_bus)
        wire.BufferBus16(dec_out[4], r4, output_bus)
        wire.BufferBus16(dec_out[5], r5, output_bus)
        wire.BufferBus16(dec_out[6], r6, output_bus)
        wire.BufferBus16(dec_out[7], r7, output_bus)
        wire.BufferBus16(dec_out[8], r8, output_bus)
        wire.BufferBus16(dec_out[9], r9, output_bus)
        wire.BufferBus16(dec_out[10], r10, output_bus)
        wire.BufferBus16(dec_out[11], r11, output_bus)
        wire.BufferBus16(dec_out[12], r12, output_bus)
        wire.BufferBus16(dec_out[13], r13, output_bus)
        wire.BufferBus16(dec_out[14], r14, output_bus)
        wire.BufferBus16(dec_out[15], r15, output_bus)


class RAM256x16:
    """Construct a new 256-word deep 16-bit wide random access memory array.
    """
    def __init__(self, data_bus, address_bus, write_enable, clock, output_bus):
        if len(data_bus) != 16:
            raise TypeError(
                "Expected bus of width 16, received bus of width {0}.".format(
                    len(data_bus)
                )
            )

        if len(address_bus) != 8:
            raise TypeError(
                "Expected bus of width 8, received bus of width {0}.".format(
                    len(address_bus)
                )
            )

        if len(output_bus) != 16:
            raise TypeError(
                "Expected bus of width 16, received bus of width {0}.".format(
                    len(output_bus)
                )
            )


class RAM65536x16:
    """Construct a new 65536-word deep 16-bit wide random access memory array.
    """
    def __init__(self, data_bus, address_bus, write_enable, clock, output_bus):
        if len(data_bus) != 16:
            raise TypeError(
                "Expected bus of width 16, received bus of width {0}.".format(
                    len(data_bus)
                )
            )

        if len(address_bus) != 16:
            raise TypeError(
                "Expected bus of width 16, received bus of width {0}.".format(
                    len(address_bus)
                )
            )

        if len(output_bus) != 16:
            raise TypeError(
                "Expected bus of width 16, received bus of width {0}.".format(
                    len(output_bus)
                )
            )
