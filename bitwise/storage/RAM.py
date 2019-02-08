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

        vcc = Wire(1)
        addr_1 = Bus4(*address_bus[0:4])
        addr_2 = Bus4(*address_bus[4:8])
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

        signal.Decoder1Of16(vcc, addr_1, dec_out)
        logic.BitwiseAND16(dec_out, wren_in, wren)

        RAM16x4(data_bus, addr_2, wren[0], clock, r0)
        RAM16x4(data_bus, addr_2, wren[1], clock, r1)
        RAM16x4(data_bus, addr_2, wren[2], clock, r2)
        RAM16x4(data_bus, addr_2, wren[3], clock, r3)
        RAM16x4(data_bus, addr_2, wren[4], clock, r4)
        RAM16x4(data_bus, addr_2, wren[5], clock, r5)
        RAM16x4(data_bus, addr_2, wren[6], clock, r6)
        RAM16x4(data_bus, addr_2, wren[7], clock, r7)
        RAM16x4(data_bus, addr_2, wren[8], clock, r8)
        RAM16x4(data_bus, addr_2, wren[9], clock, r9)
        RAM16x4(data_bus, addr_2, wren[10], clock, r10)
        RAM16x4(data_bus, addr_2, wren[11], clock, r11)
        RAM16x4(data_bus, addr_2, wren[12], clock, r12)
        RAM16x4(data_bus, addr_2, wren[13], clock, r13)
        RAM16x4(data_bus, addr_2, wren[14], clock, r14)
        RAM16x4(data_bus, addr_2, wren[15], clock, r15)

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

        vcc = Wire(1)
        addr_1 = Bus4(*address_bus[0:4])
        addr_2 = Bus4(*address_bus[4:8])
        addr_3 = Bus4(*address_bus[8:16])
        dec_1_out = Bus16()
        dec_2_out = Bus16()
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
        r16 = Bus4()
        r17 = Bus4()
        r18 = Bus4()
        r19 = Bus4()
        r20 = Bus4()
        r21 = Bus4()
        r22 = Bus4()
        r23 = Bus4()
        r24 = Bus4()
        r25 = Bus4()
        r26 = Bus4()
        r27 = Bus4()
        r28 = Bus4()
        r29 = Bus4()
        r30 = Bus4()
        r31 = Bus4()
        r32 = Bus4()
        r33 = Bus4()
        r34 = Bus4()
        r35 = Bus4()
        r36 = Bus4()
        r37 = Bus4()
        r38 = Bus4()
        r39 = Bus4()
        r40 = Bus4()
        r41 = Bus4()
        r42 = Bus4()
        r43 = Bus4()
        r44 = Bus4()
        r45 = Bus4()
        r46 = Bus4()
        r47 = Bus4()
        r48 = Bus4()
        r49 = Bus4()
        r50 = Bus4()
        r51 = Bus4()
        r52 = Bus4()
        r53 = Bus4()
        r54 = Bus4()
        r55 = Bus4()
        r56 = Bus4()
        r57 = Bus4()
        r58 = Bus4()
        r59 = Bus4()
        r60 = Bus4()
        r61 = Bus4()
        r62 = Bus4()
        r63 = Bus4()
        r64 = Bus4()
        r65 = Bus4()
        r66 = Bus4()
        r67 = Bus4()
        r68 = Bus4()
        r69 = Bus4()
        r70 = Bus4()
        r71 = Bus4()
        r72 = Bus4()
        r73 = Bus4()
        r74 = Bus4()
        r75 = Bus4()
        r76 = Bus4()
        r77 = Bus4()
        r78 = Bus4()
        r79 = Bus4()
        r80 = Bus4()
        r81 = Bus4()
        r82 = Bus4()
        r83 = Bus4()
        r84 = Bus4()
        r85 = Bus4()
        r86 = Bus4()
        r87 = Bus4()
        r88 = Bus4()
        r89 = Bus4()
        r90 = Bus4()
        r91 = Bus4()
        r92 = Bus4()
        r93 = Bus4()
        r94 = Bus4()
        r95 = Bus4()
        r96 = Bus4()
        r97 = Bus4()
        r98 = Bus4()
        r99 = Bus4()
        r100 = Bus4()
        r101 = Bus4()
        r102 = Bus4()
        r103 = Bus4()
        r104 = Bus4()
        r105 = Bus4()
        r106 = Bus4()
        r107 = Bus4()
        r108 = Bus4()
        r109 = Bus4()
        r110 = Bus4()
        r111 = Bus4()
        r112 = Bus4()
        r113 = Bus4()
        r114 = Bus4()
        r115 = Bus4()
        r116 = Bus4()
        r117 = Bus4()
        r118 = Bus4()
        r119 = Bus4()
        r120 = Bus4()
        r121 = Bus4()
        r122 = Bus4()
        r123 = Bus4()
        r124 = Bus4()
        r125 = Bus4()
        r126 = Bus4()
        r127 = Bus4()
        r128 = Bus4()
        r129 = Bus4()
        r130 = Bus4()
        r131 = Bus4()
        r132 = Bus4()
        r133 = Bus4()
        r134 = Bus4()
        r135 = Bus4()
        r136 = Bus4()
        r137 = Bus4()
        r138 = Bus4()
        r139 = Bus4()
        r140 = Bus4()
        r141 = Bus4()
        r142 = Bus4()
        r143 = Bus4()
        r144 = Bus4()
        r145 = Bus4()
        r146 = Bus4()
        r147 = Bus4()
        r148 = Bus4()
        r149 = Bus4()
        r150 = Bus4()
        r151 = Bus4()
        r152 = Bus4()
        r153 = Bus4()
        r154 = Bus4()
        r155 = Bus4()
        r156 = Bus4()
        r157 = Bus4()
        r158 = Bus4()
        r159 = Bus4()
        r160 = Bus4()
        r161 = Bus4()
        r162 = Bus4()
        r163 = Bus4()
        r164 = Bus4()
        r165 = Bus4()
        r166 = Bus4()
        r167 = Bus4()
        r168 = Bus4()
        r169 = Bus4()
        r170 = Bus4()
        r171 = Bus4()
        r172 = Bus4()
        r173 = Bus4()
        r174 = Bus4()
        r175 = Bus4()
        r176 = Bus4()
        r177 = Bus4()
        r178 = Bus4()
        r179 = Bus4()
        r180 = Bus4()
        r181 = Bus4()
        r182 = Bus4()
        r183 = Bus4()
        r184 = Bus4()
        r185 = Bus4()
        r186 = Bus4()
        r187 = Bus4()
        r188 = Bus4()
        r189 = Bus4()
        r190 = Bus4()
        r191 = Bus4()
        r192 = Bus4()
        r193 = Bus4()
        r194 = Bus4()
        r195 = Bus4()
        r196 = Bus4()
        r197 = Bus4()
        r198 = Bus4()
        r199 = Bus4()
        r200 = Bus4()
        r201 = Bus4()
        r202 = Bus4()
        r203 = Bus4()
        r204 = Bus4()
        r205 = Bus4()
        r206 = Bus4()
        r207 = Bus4()
        r208 = Bus4()
        r209 = Bus4()
        r210 = Bus4()
        r211 = Bus4()
        r212 = Bus4()
        r213 = Bus4()
        r214 = Bus4()
        r215 = Bus4()
        r216 = Bus4()
        r217 = Bus4()
        r218 = Bus4()
        r219 = Bus4()
        r220 = Bus4()
        r221 = Bus4()
        r222 = Bus4()
        r223 = Bus4()
        r224 = Bus4()
        r225 = Bus4()
        r226 = Bus4()
        r227 = Bus4()
        r228 = Bus4()
        r229 = Bus4()
        r230 = Bus4()
        r231 = Bus4()
        r232 = Bus4()
        r233 = Bus4()
        r234 = Bus4()
        r235 = Bus4()
        r236 = Bus4()
        r237 = Bus4()
        r238 = Bus4()
        r239 = Bus4()
        r240 = Bus4()
        r241 = Bus4()
        r242 = Bus4()
        r243 = Bus4()
        r244 = Bus4()
        r245 = Bus4()
        r246 = Bus4()
        r247 = Bus4()
        r248 = Bus4()
        r249 = Bus4()
        r250 = Bus4()
        r251 = Bus4()
        r252 = Bus4()
        r253 = Bus4()
        r254 = Bus4()
        r255 = Bus4()

        signal.Decoder1Of16(vcc, addr_1, dec_1_out)
        signal.Decoder1Of16(vcc, addr_2, dec_2_out)
        logic.BitwiseAND16(dec_1_out, wren_in, wren)

        RAM256x4(data_bus, addr_3, wren[0], clock, r0)
        RAM256x4(data_bus, addr_3, wren[1], clock, r1)
        RAM256x4(data_bus, addr_3, wren[2], clock, r2)
        RAM256x4(data_bus, addr_3, wren[3], clock, r3)
        RAM256x4(data_bus, addr_3, wren[4], clock, r4)
        RAM256x4(data_bus, addr_3, wren[5], clock, r5)
        RAM256x4(data_bus, addr_3, wren[6], clock, r6)
        RAM256x4(data_bus, addr_3, wren[7], clock, r7)
        RAM256x4(data_bus, addr_3, wren[8], clock, r8)
        RAM256x4(data_bus, addr_3, wren[9], clock, r9)
        RAM256x4(data_bus, addr_3, wren[10], clock, r10)
        RAM256x4(data_bus, addr_3, wren[11], clock, r11)
        RAM256x4(data_bus, addr_3, wren[12], clock, r12)
        RAM256x4(data_bus, addr_3, wren[13], clock, r13)
        RAM256x4(data_bus, addr_3, wren[14], clock, r14)
        RAM256x4(data_bus, addr_3, wren[15], clock, r15)
        RAM256x4(data_bus, addr_3, wren[0], clock, r16)
        RAM256x4(data_bus, addr_3, wren[1], clock, r17)
        RAM256x4(data_bus, addr_3, wren[2], clock, r18)
        RAM256x4(data_bus, addr_3, wren[3], clock, r19)
        RAM256x4(data_bus, addr_3, wren[4], clock, r20)
        RAM256x4(data_bus, addr_3, wren[5], clock, r21)
        RAM256x4(data_bus, addr_3, wren[6], clock, r22)
        RAM256x4(data_bus, addr_3, wren[7], clock, r23)
        RAM256x4(data_bus, addr_3, wren[8], clock, r24)
        RAM256x4(data_bus, addr_3, wren[9], clock, r25)
        RAM256x4(data_bus, addr_3, wren[10], clock, r26)
        RAM256x4(data_bus, addr_3, wren[11], clock, r27)
        RAM256x4(data_bus, addr_3, wren[12], clock, r28)
        RAM256x4(data_bus, addr_3, wren[13], clock, r29)
        RAM256x4(data_bus, addr_3, wren[14], clock, r30)
        RAM256x4(data_bus, addr_3, wren[15], clock, r31)
        RAM256x4(data_bus, addr_3, wren[0], clock, r32)
        RAM256x4(data_bus, addr_3, wren[1], clock, r33)
        RAM256x4(data_bus, addr_3, wren[2], clock, r34)
        RAM256x4(data_bus, addr_3, wren[3], clock, r35)
        RAM256x4(data_bus, addr_3, wren[4], clock, r36)
        RAM256x4(data_bus, addr_3, wren[5], clock, r37)
        RAM256x4(data_bus, addr_3, wren[6], clock, r38)
        RAM256x4(data_bus, addr_3, wren[7], clock, r39)
        RAM256x4(data_bus, addr_3, wren[8], clock, r40)
        RAM256x4(data_bus, addr_3, wren[9], clock, r41)
        RAM256x4(data_bus, addr_3, wren[10], clock, r42)
        RAM256x4(data_bus, addr_3, wren[11], clock, r43)
        RAM256x4(data_bus, addr_3, wren[12], clock, r44)
        RAM256x4(data_bus, addr_3, wren[13], clock, r45)
        RAM256x4(data_bus, addr_3, wren[14], clock, r46)
        RAM256x4(data_bus, addr_3, wren[15], clock, r47)
        RAM256x4(data_bus, addr_3, wren[0], clock, r48)
        RAM256x4(data_bus, addr_3, wren[1], clock, r49)
        RAM256x4(data_bus, addr_3, wren[2], clock, r50)
        RAM256x4(data_bus, addr_3, wren[3], clock, r51)
        RAM256x4(data_bus, addr_3, wren[4], clock, r52)
        RAM256x4(data_bus, addr_3, wren[5], clock, r53)
        RAM256x4(data_bus, addr_3, wren[6], clock, r54)
        RAM256x4(data_bus, addr_3, wren[7], clock, r55)
        RAM256x4(data_bus, addr_3, wren[8], clock, r56)
        RAM256x4(data_bus, addr_3, wren[9], clock, r57)
        RAM256x4(data_bus, addr_3, wren[10], clock, r58)
        RAM256x4(data_bus, addr_3, wren[11], clock, r59)
        RAM256x4(data_bus, addr_3, wren[12], clock, r60)
        RAM256x4(data_bus, addr_3, wren[13], clock, r61)
        RAM256x4(data_bus, addr_3, wren[14], clock, r62)
        RAM256x4(data_bus, addr_3, wren[15], clock, r63)
        RAM256x4(data_bus, addr_3, wren[0], clock, r64)
        RAM256x4(data_bus, addr_3, wren[1], clock, r65)
        RAM256x4(data_bus, addr_3, wren[2], clock, r66)
        RAM256x4(data_bus, addr_3, wren[3], clock, r67)
        RAM256x4(data_bus, addr_3, wren[4], clock, r68)
        RAM256x4(data_bus, addr_3, wren[5], clock, r69)
        RAM256x4(data_bus, addr_3, wren[6], clock, r70)
        RAM256x4(data_bus, addr_3, wren[7], clock, r71)
        RAM256x4(data_bus, addr_3, wren[8], clock, r72)
        RAM256x4(data_bus, addr_3, wren[9], clock, r73)
        RAM256x4(data_bus, addr_3, wren[10], clock, r74)
        RAM256x4(data_bus, addr_3, wren[11], clock, r75)
        RAM256x4(data_bus, addr_3, wren[12], clock, r76)
        RAM256x4(data_bus, addr_3, wren[13], clock, r77)
        RAM256x4(data_bus, addr_3, wren[14], clock, r78)
        RAM256x4(data_bus, addr_3, wren[15], clock, r79)
        RAM256x4(data_bus, addr_3, wren[0], clock, r80)
        RAM256x4(data_bus, addr_3, wren[1], clock, r81)
        RAM256x4(data_bus, addr_3, wren[2], clock, r82)
        RAM256x4(data_bus, addr_3, wren[3], clock, r83)
        RAM256x4(data_bus, addr_3, wren[4], clock, r84)
        RAM256x4(data_bus, addr_3, wren[5], clock, r85)
        RAM256x4(data_bus, addr_3, wren[6], clock, r86)
        RAM256x4(data_bus, addr_3, wren[7], clock, r87)
        RAM256x4(data_bus, addr_3, wren[8], clock, r88)
        RAM256x4(data_bus, addr_3, wren[9], clock, r89)
        RAM256x4(data_bus, addr_3, wren[10], clock, r90)
        RAM256x4(data_bus, addr_3, wren[11], clock, r91)
        RAM256x4(data_bus, addr_3, wren[12], clock, r92)
        RAM256x4(data_bus, addr_3, wren[13], clock, r93)
        RAM256x4(data_bus, addr_3, wren[14], clock, r94)
        RAM256x4(data_bus, addr_3, wren[15], clock, r95)
        RAM256x4(data_bus, addr_3, wren[0], clock, r96)
        RAM256x4(data_bus, addr_3, wren[1], clock, r97)
        RAM256x4(data_bus, addr_3, wren[2], clock, r98)
        RAM256x4(data_bus, addr_3, wren[3], clock, r99)
        RAM256x4(data_bus, addr_3, wren[4], clock, r100)
        RAM256x4(data_bus, addr_3, wren[5], clock, r101)
        RAM256x4(data_bus, addr_3, wren[6], clock, r102)
        RAM256x4(data_bus, addr_3, wren[7], clock, r103)
        RAM256x4(data_bus, addr_3, wren[8], clock, r104)
        RAM256x4(data_bus, addr_3, wren[9], clock, r105)
        RAM256x4(data_bus, addr_3, wren[10], clock, r106)
        RAM256x4(data_bus, addr_3, wren[11], clock, r107)
        RAM256x4(data_bus, addr_3, wren[12], clock, r108)
        RAM256x4(data_bus, addr_3, wren[13], clock, r109)
        RAM256x4(data_bus, addr_3, wren[14], clock, r110)
        RAM256x4(data_bus, addr_3, wren[15], clock, r111)
        RAM256x4(data_bus, addr_3, wren[0], clock, r112)
        RAM256x4(data_bus, addr_3, wren[1], clock, r113)
        RAM256x4(data_bus, addr_3, wren[2], clock, r114)
        RAM256x4(data_bus, addr_3, wren[3], clock, r115)
        RAM256x4(data_bus, addr_3, wren[4], clock, r116)
        RAM256x4(data_bus, addr_3, wren[5], clock, r117)
        RAM256x4(data_bus, addr_3, wren[6], clock, r118)
        RAM256x4(data_bus, addr_3, wren[7], clock, r119)
        RAM256x4(data_bus, addr_3, wren[8], clock, r120)
        RAM256x4(data_bus, addr_3, wren[9], clock, r121)
        RAM256x4(data_bus, addr_3, wren[10], clock, r122)
        RAM256x4(data_bus, addr_3, wren[11], clock, r123)
        RAM256x4(data_bus, addr_3, wren[12], clock, r124)
        RAM256x4(data_bus, addr_3, wren[13], clock, r125)
        RAM256x4(data_bus, addr_3, wren[14], clock, r126)
        RAM256x4(data_bus, addr_3, wren[15], clock, r127)
        RAM256x4(data_bus, addr_3, wren[0], clock, r128)
        RAM256x4(data_bus, addr_3, wren[1], clock, r129)
        RAM256x4(data_bus, addr_3, wren[2], clock, r130)
        RAM256x4(data_bus, addr_3, wren[3], clock, r131)
        RAM256x4(data_bus, addr_3, wren[4], clock, r132)
        RAM256x4(data_bus, addr_3, wren[5], clock, r133)
        RAM256x4(data_bus, addr_3, wren[6], clock, r134)
        RAM256x4(data_bus, addr_3, wren[7], clock, r135)
        RAM256x4(data_bus, addr_3, wren[8], clock, r136)
        RAM256x4(data_bus, addr_3, wren[9], clock, r137)
        RAM256x4(data_bus, addr_3, wren[10], clock, r138)
        RAM256x4(data_bus, addr_3, wren[11], clock, r139)
        RAM256x4(data_bus, addr_3, wren[12], clock, r140)
        RAM256x4(data_bus, addr_3, wren[13], clock, r141)
        RAM256x4(data_bus, addr_3, wren[14], clock, r142)
        RAM256x4(data_bus, addr_3, wren[15], clock, r143)
        RAM256x4(data_bus, addr_3, wren[0], clock, r144)
        RAM256x4(data_bus, addr_3, wren[1], clock, r145)
        RAM256x4(data_bus, addr_3, wren[2], clock, r146)
        RAM256x4(data_bus, addr_3, wren[3], clock, r147)
        RAM256x4(data_bus, addr_3, wren[4], clock, r148)
        RAM256x4(data_bus, addr_3, wren[5], clock, r149)
        RAM256x4(data_bus, addr_3, wren[6], clock, r150)
        RAM256x4(data_bus, addr_3, wren[7], clock, r151)
        RAM256x4(data_bus, addr_3, wren[8], clock, r152)
        RAM256x4(data_bus, addr_3, wren[9], clock, r153)
        RAM256x4(data_bus, addr_3, wren[10], clock, r154)
        RAM256x4(data_bus, addr_3, wren[11], clock, r155)
        RAM256x4(data_bus, addr_3, wren[12], clock, r156)
        RAM256x4(data_bus, addr_3, wren[13], clock, r157)
        RAM256x4(data_bus, addr_3, wren[14], clock, r158)
        RAM256x4(data_bus, addr_3, wren[15], clock, r159)
        RAM256x4(data_bus, addr_3, wren[0], clock, r160)
        RAM256x4(data_bus, addr_3, wren[1], clock, r161)
        RAM256x4(data_bus, addr_3, wren[2], clock, r162)
        RAM256x4(data_bus, addr_3, wren[3], clock, r163)
        RAM256x4(data_bus, addr_3, wren[4], clock, r164)
        RAM256x4(data_bus, addr_3, wren[5], clock, r165)
        RAM256x4(data_bus, addr_3, wren[6], clock, r166)
        RAM256x4(data_bus, addr_3, wren[7], clock, r167)
        RAM256x4(data_bus, addr_3, wren[8], clock, r168)
        RAM256x4(data_bus, addr_3, wren[9], clock, r169)
        RAM256x4(data_bus, addr_3, wren[10], clock, r170)
        RAM256x4(data_bus, addr_3, wren[11], clock, r171)
        RAM256x4(data_bus, addr_3, wren[12], clock, r172)
        RAM256x4(data_bus, addr_3, wren[13], clock, r173)
        RAM256x4(data_bus, addr_3, wren[14], clock, r174)
        RAM256x4(data_bus, addr_3, wren[15], clock, r175)
        RAM256x4(data_bus, addr_3, wren[0], clock, r176)
        RAM256x4(data_bus, addr_3, wren[1], clock, r177)
        RAM256x4(data_bus, addr_3, wren[2], clock, r178)
        RAM256x4(data_bus, addr_3, wren[3], clock, r179)
        RAM256x4(data_bus, addr_3, wren[4], clock, r180)
        RAM256x4(data_bus, addr_3, wren[5], clock, r181)
        RAM256x4(data_bus, addr_3, wren[6], clock, r182)
        RAM256x4(data_bus, addr_3, wren[7], clock, r183)
        RAM256x4(data_bus, addr_3, wren[8], clock, r184)
        RAM256x4(data_bus, addr_3, wren[9], clock, r185)
        RAM256x4(data_bus, addr_3, wren[10], clock, r186)
        RAM256x4(data_bus, addr_3, wren[11], clock, r187)
        RAM256x4(data_bus, addr_3, wren[12], clock, r188)
        RAM256x4(data_bus, addr_3, wren[13], clock, r189)
        RAM256x4(data_bus, addr_3, wren[14], clock, r190)
        RAM256x4(data_bus, addr_3, wren[15], clock, r191)
        RAM256x4(data_bus, addr_3, wren[0], clock, r192)
        RAM256x4(data_bus, addr_3, wren[1], clock, r193)
        RAM256x4(data_bus, addr_3, wren[2], clock, r194)
        RAM256x4(data_bus, addr_3, wren[3], clock, r195)
        RAM256x4(data_bus, addr_3, wren[4], clock, r196)
        RAM256x4(data_bus, addr_3, wren[5], clock, r197)
        RAM256x4(data_bus, addr_3, wren[6], clock, r198)
        RAM256x4(data_bus, addr_3, wren[7], clock, r199)
        RAM256x4(data_bus, addr_3, wren[8], clock, r200)
        RAM256x4(data_bus, addr_3, wren[9], clock, r201)
        RAM256x4(data_bus, addr_3, wren[10], clock, r202)
        RAM256x4(data_bus, addr_3, wren[11], clock, r203)
        RAM256x4(data_bus, addr_3, wren[12], clock, r204)
        RAM256x4(data_bus, addr_3, wren[13], clock, r205)
        RAM256x4(data_bus, addr_3, wren[14], clock, r206)
        RAM256x4(data_bus, addr_3, wren[15], clock, r207)
        RAM256x4(data_bus, addr_3, wren[0], clock, r208)
        RAM256x4(data_bus, addr_3, wren[1], clock, r209)
        RAM256x4(data_bus, addr_3, wren[2], clock, r210)
        RAM256x4(data_bus, addr_3, wren[3], clock, r211)
        RAM256x4(data_bus, addr_3, wren[4], clock, r212)
        RAM256x4(data_bus, addr_3, wren[5], clock, r213)
        RAM256x4(data_bus, addr_3, wren[6], clock, r214)
        RAM256x4(data_bus, addr_3, wren[7], clock, r215)
        RAM256x4(data_bus, addr_3, wren[8], clock, r216)
        RAM256x4(data_bus, addr_3, wren[9], clock, r217)
        RAM256x4(data_bus, addr_3, wren[10], clock, r218)
        RAM256x4(data_bus, addr_3, wren[11], clock, r219)
        RAM256x4(data_bus, addr_3, wren[12], clock, r220)
        RAM256x4(data_bus, addr_3, wren[13], clock, r221)
        RAM256x4(data_bus, addr_3, wren[14], clock, r222)
        RAM256x4(data_bus, addr_3, wren[15], clock, r223)
        RAM256x4(data_bus, addr_3, wren[0], clock, r224)
        RAM256x4(data_bus, addr_3, wren[1], clock, r225)
        RAM256x4(data_bus, addr_3, wren[2], clock, r226)
        RAM256x4(data_bus, addr_3, wren[3], clock, r227)
        RAM256x4(data_bus, addr_3, wren[4], clock, r228)
        RAM256x4(data_bus, addr_3, wren[5], clock, r229)
        RAM256x4(data_bus, addr_3, wren[6], clock, r230)
        RAM256x4(data_bus, addr_3, wren[7], clock, r231)
        RAM256x4(data_bus, addr_3, wren[8], clock, r232)
        RAM256x4(data_bus, addr_3, wren[9], clock, r233)
        RAM256x4(data_bus, addr_3, wren[10], clock, r234)
        RAM256x4(data_bus, addr_3, wren[11], clock, r235)
        RAM256x4(data_bus, addr_3, wren[12], clock, r236)
        RAM256x4(data_bus, addr_3, wren[13], clock, r237)
        RAM256x4(data_bus, addr_3, wren[14], clock, r238)
        RAM256x4(data_bus, addr_3, wren[15], clock, r239)
        RAM256x4(data_bus, addr_3, wren[0], clock, r240)
        RAM256x4(data_bus, addr_3, wren[1], clock, r241)
        RAM256x4(data_bus, addr_3, wren[2], clock, r242)
        RAM256x4(data_bus, addr_3, wren[3], clock, r243)
        RAM256x4(data_bus, addr_3, wren[4], clock, r244)
        RAM256x4(data_bus, addr_3, wren[5], clock, r245)
        RAM256x4(data_bus, addr_3, wren[6], clock, r246)
        RAM256x4(data_bus, addr_3, wren[7], clock, r247)
        RAM256x4(data_bus, addr_3, wren[8], clock, r248)
        RAM256x4(data_bus, addr_3, wren[9], clock, r249)
        RAM256x4(data_bus, addr_3, wren[10], clock, r250)
        RAM256x4(data_bus, addr_3, wren[11], clock, r251)
        RAM256x4(data_bus, addr_3, wren[12], clock, r252)
        RAM256x4(data_bus, addr_3, wren[13], clock, r253)
        RAM256x4(data_bus, addr_3, wren[14], clock, r254)
        RAM256x4(data_bus, addr_3, wren[15], clock, r255)

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

        vcc = Wire(1)
        addr_1 = Bus4(*address_bus[0:4])
        addr_2 = Bus4(*address_bus[4:8])
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

        signal.Decoder1Of16(vcc, addr_1, dec_out)
        logic.BitwiseAND16(dec_out, wren_in, wren)

        RAM16x8(data_bus, addr_2, wren[0], clock, r0)
        RAM16x8(data_bus, addr_2, wren[1], clock, r1)
        RAM16x8(data_bus, addr_2, wren[2], clock, r2)
        RAM16x8(data_bus, addr_2, wren[3], clock, r3)
        RAM16x8(data_bus, addr_2, wren[4], clock, r4)
        RAM16x8(data_bus, addr_2, wren[5], clock, r5)
        RAM16x8(data_bus, addr_2, wren[6], clock, r6)
        RAM16x8(data_bus, addr_2, wren[7], clock, r7)
        RAM16x8(data_bus, addr_2, wren[8], clock, r8)
        RAM16x8(data_bus, addr_2, wren[9], clock, r9)
        RAM16x8(data_bus, addr_2, wren[10], clock, r10)
        RAM16x8(data_bus, addr_2, wren[11], clock, r11)
        RAM16x8(data_bus, addr_2, wren[12], clock, r12)
        RAM16x8(data_bus, addr_2, wren[13], clock, r13)
        RAM16x8(data_bus, addr_2, wren[14], clock, r14)
        RAM16x8(data_bus, addr_2, wren[15], clock, r15)

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

        vcc = Wire(1)
        addr_1 = Bus4(*address_bus[0:4])
        addr_2 = Bus4(*address_bus[4:8])
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

        signal.Decoder1Of16(vcc, addr_1, dec_out)
        logic.BitwiseAND16(dec_out, wren_in, wren)

        RAM16x16(data_bus, addr_2, wren[0], clock, r0)
        RAM16x16(data_bus, addr_2, wren[1], clock, r1)
        RAM16x16(data_bus, addr_2, wren[2], clock, r2)
        RAM16x16(data_bus, addr_2, wren[3], clock, r3)
        RAM16x16(data_bus, addr_2, wren[4], clock, r4)
        RAM16x16(data_bus, addr_2, wren[5], clock, r5)
        RAM16x16(data_bus, addr_2, wren[6], clock, r6)
        RAM16x16(data_bus, addr_2, wren[7], clock, r7)
        RAM16x16(data_bus, addr_2, wren[8], clock, r8)
        RAM16x16(data_bus, addr_2, wren[9], clock, r9)
        RAM16x16(data_bus, addr_2, wren[10], clock, r10)
        RAM16x16(data_bus, addr_2, wren[11], clock, r11)
        RAM16x16(data_bus, addr_2, wren[12], clock, r12)
        RAM16x16(data_bus, addr_2, wren[13], clock, r13)
        RAM16x16(data_bus, addr_2, wren[14], clock, r14)
        RAM16x16(data_bus, addr_2, wren[15], clock, r15)

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
