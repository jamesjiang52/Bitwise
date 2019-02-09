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
from .. import gate
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
        wren = write_enable
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

        gate.ANDGate3(wren, dec_1_out[0], dec_2_out[0], wren_1_1)
        gate.ANDGate3(wren, dec_1_out[1], dec_2_out[0], wren_2_1)
        gate.ANDGate3(wren, dec_1_out[2], dec_2_out[0], wren_3_1)
        gate.ANDGate3(wren, dec_1_out[3], dec_2_out[0], wren_4_1)
        gate.ANDGate3(wren, dec_1_out[4], dec_2_out[0], wren_5_1)
        gate.ANDGate3(wren, dec_1_out[5], dec_2_out[0], wren_6_1)
        gate.ANDGate3(wren, dec_1_out[6], dec_2_out[0], wren_7_1)
        gate.ANDGate3(wren, dec_1_out[7], dec_2_out[0], wren_8_1)
        gate.ANDGate3(wren, dec_1_out[8], dec_2_out[0], wren_9_1)
        gate.ANDGate3(wren, dec_1_out[9], dec_2_out[0], wren_10_1)
        gate.ANDGate3(wren, dec_1_out[10], dec_2_out[0], wren_11_1)
        gate.ANDGate3(wren, dec_1_out[11], dec_2_out[0], wren_12_1)
        gate.ANDGate3(wren, dec_1_out[12], dec_2_out[0], wren_13_1)
        gate.ANDGate3(wren, dec_1_out[13], dec_2_out[0], wren_14_1)
        gate.ANDGate3(wren, dec_1_out[14], dec_2_out[0], wren_15_1)
        gate.ANDGate3(wren, dec_1_out[15], dec_2_out[0], wren_16_1)
        gate.ANDGate3(wren, dec_1_out[0], dec_2_out[1], wren_1_2)
        gate.ANDGate3(wren, dec_1_out[1], dec_2_out[1], wren_2_2)
        gate.ANDGate3(wren, dec_1_out[2], dec_2_out[1], wren_3_2)
        gate.ANDGate3(wren, dec_1_out[3], dec_2_out[1], wren_4_2)
        gate.ANDGate3(wren, dec_1_out[4], dec_2_out[1], wren_5_2)
        gate.ANDGate3(wren, dec_1_out[5], dec_2_out[1], wren_6_2)
        gate.ANDGate3(wren, dec_1_out[6], dec_2_out[1], wren_7_2)
        gate.ANDGate3(wren, dec_1_out[7], dec_2_out[1], wren_8_2)
        gate.ANDGate3(wren, dec_1_out[8], dec_2_out[1], wren_9_2)
        gate.ANDGate3(wren, dec_1_out[9], dec_2_out[1], wren_10_2)
        gate.ANDGate3(wren, dec_1_out[10], dec_2_out[1], wren_11_2)
        gate.ANDGate3(wren, dec_1_out[11], dec_2_out[1], wren_12_2)
        gate.ANDGate3(wren, dec_1_out[12], dec_2_out[1], wren_13_2)
        gate.ANDGate3(wren, dec_1_out[13], dec_2_out[1], wren_14_2)
        gate.ANDGate3(wren, dec_1_out[14], dec_2_out[1], wren_15_2)
        gate.ANDGate3(wren, dec_1_out[15], dec_2_out[1], wren_16_2)
        gate.ANDGate3(wren, dec_1_out[0], dec_2_out[2], wren_1_3)
        gate.ANDGate3(wren, dec_1_out[1], dec_2_out[2], wren_2_3)
        gate.ANDGate3(wren, dec_1_out[2], dec_2_out[2], wren_3_3)
        gate.ANDGate3(wren, dec_1_out[3], dec_2_out[2], wren_4_3)
        gate.ANDGate3(wren, dec_1_out[4], dec_2_out[2], wren_5_3)
        gate.ANDGate3(wren, dec_1_out[5], dec_2_out[2], wren_6_3)
        gate.ANDGate3(wren, dec_1_out[6], dec_2_out[2], wren_7_3)
        gate.ANDGate3(wren, dec_1_out[7], dec_2_out[2], wren_8_3)
        gate.ANDGate3(wren, dec_1_out[8], dec_2_out[2], wren_9_3)
        gate.ANDGate3(wren, dec_1_out[9], dec_2_out[2], wren_10_3)
        gate.ANDGate3(wren, dec_1_out[10], dec_2_out[2], wren_11_3)
        gate.ANDGate3(wren, dec_1_out[11], dec_2_out[2], wren_12_3)
        gate.ANDGate3(wren, dec_1_out[12], dec_2_out[2], wren_13_3)
        gate.ANDGate3(wren, dec_1_out[13], dec_2_out[2], wren_14_3)
        gate.ANDGate3(wren, dec_1_out[14], dec_2_out[2], wren_15_3)
        gate.ANDGate3(wren, dec_1_out[15], dec_2_out[2], wren_16_3)
        gate.ANDGate3(wren, dec_1_out[0], dec_2_out[3], wren_1_4)
        gate.ANDGate3(wren, dec_1_out[1], dec_2_out[3], wren_2_4)
        gate.ANDGate3(wren, dec_1_out[2], dec_2_out[3], wren_3_4)
        gate.ANDGate3(wren, dec_1_out[3], dec_2_out[3], wren_4_4)
        gate.ANDGate3(wren, dec_1_out[4], dec_2_out[3], wren_5_4)
        gate.ANDGate3(wren, dec_1_out[5], dec_2_out[3], wren_6_4)
        gate.ANDGate3(wren, dec_1_out[6], dec_2_out[3], wren_7_4)
        gate.ANDGate3(wren, dec_1_out[7], dec_2_out[3], wren_8_4)
        gate.ANDGate3(wren, dec_1_out[8], dec_2_out[3], wren_9_4)
        gate.ANDGate3(wren, dec_1_out[9], dec_2_out[3], wren_10_4)
        gate.ANDGate3(wren, dec_1_out[10], dec_2_out[3], wren_11_4)
        gate.ANDGate3(wren, dec_1_out[11], dec_2_out[3], wren_12_4)
        gate.ANDGate3(wren, dec_1_out[12], dec_2_out[3], wren_13_4)
        gate.ANDGate3(wren, dec_1_out[13], dec_2_out[3], wren_14_4)
        gate.ANDGate3(wren, dec_1_out[14], dec_2_out[3], wren_15_4)
        gate.ANDGate3(wren, dec_1_out[15], dec_2_out[3], wren_16_4)
        gate.ANDGate3(wren, dec_1_out[0], dec_2_out[4], wren_1_5)
        gate.ANDGate3(wren, dec_1_out[1], dec_2_out[4], wren_2_5)
        gate.ANDGate3(wren, dec_1_out[2], dec_2_out[4], wren_3_5)
        gate.ANDGate3(wren, dec_1_out[3], dec_2_out[4], wren_4_5)
        gate.ANDGate3(wren, dec_1_out[4], dec_2_out[4], wren_5_5)
        gate.ANDGate3(wren, dec_1_out[5], dec_2_out[4], wren_6_5)
        gate.ANDGate3(wren, dec_1_out[6], dec_2_out[4], wren_7_5)
        gate.ANDGate3(wren, dec_1_out[7], dec_2_out[4], wren_8_5)
        gate.ANDGate3(wren, dec_1_out[8], dec_2_out[4], wren_9_5)
        gate.ANDGate3(wren, dec_1_out[9], dec_2_out[4], wren_10_5)
        gate.ANDGate3(wren, dec_1_out[10], dec_2_out[4], wren_11_5)
        gate.ANDGate3(wren, dec_1_out[11], dec_2_out[4], wren_12_5)
        gate.ANDGate3(wren, dec_1_out[12], dec_2_out[4], wren_13_5)
        gate.ANDGate3(wren, dec_1_out[13], dec_2_out[4], wren_14_5)
        gate.ANDGate3(wren, dec_1_out[14], dec_2_out[4], wren_15_5)
        gate.ANDGate3(wren, dec_1_out[15], dec_2_out[4], wren_16_5)
        gate.ANDGate3(wren, dec_1_out[0], dec_2_out[5], wren_1_6)
        gate.ANDGate3(wren, dec_1_out[1], dec_2_out[5], wren_2_6)
        gate.ANDGate3(wren, dec_1_out[2], dec_2_out[5], wren_3_6)
        gate.ANDGate3(wren, dec_1_out[3], dec_2_out[5], wren_4_6)
        gate.ANDGate3(wren, dec_1_out[4], dec_2_out[5], wren_5_6)
        gate.ANDGate3(wren, dec_1_out[5], dec_2_out[5], wren_6_6)
        gate.ANDGate3(wren, dec_1_out[6], dec_2_out[5], wren_7_6)
        gate.ANDGate3(wren, dec_1_out[7], dec_2_out[5], wren_8_6)
        gate.ANDGate3(wren, dec_1_out[8], dec_2_out[5], wren_9_6)
        gate.ANDGate3(wren, dec_1_out[9], dec_2_out[5], wren_10_6)
        gate.ANDGate3(wren, dec_1_out[10], dec_2_out[5], wren_11_6)
        gate.ANDGate3(wren, dec_1_out[11], dec_2_out[5], wren_12_6)
        gate.ANDGate3(wren, dec_1_out[12], dec_2_out[5], wren_13_6)
        gate.ANDGate3(wren, dec_1_out[13], dec_2_out[5], wren_14_6)
        gate.ANDGate3(wren, dec_1_out[14], dec_2_out[5], wren_15_6)
        gate.ANDGate3(wren, dec_1_out[15], dec_2_out[5], wren_16_6)
        gate.ANDGate3(wren, dec_1_out[0], dec_2_out[6], wren_1_7)
        gate.ANDGate3(wren, dec_1_out[1], dec_2_out[6], wren_2_7)
        gate.ANDGate3(wren, dec_1_out[2], dec_2_out[6], wren_3_7)
        gate.ANDGate3(wren, dec_1_out[3], dec_2_out[6], wren_4_7)
        gate.ANDGate3(wren, dec_1_out[4], dec_2_out[6], wren_5_7)
        gate.ANDGate3(wren, dec_1_out[5], dec_2_out[6], wren_6_7)
        gate.ANDGate3(wren, dec_1_out[6], dec_2_out[6], wren_7_7)
        gate.ANDGate3(wren, dec_1_out[7], dec_2_out[6], wren_8_7)
        gate.ANDGate3(wren, dec_1_out[8], dec_2_out[6], wren_9_7)
        gate.ANDGate3(wren, dec_1_out[9], dec_2_out[6], wren_10_7)
        gate.ANDGate3(wren, dec_1_out[10], dec_2_out[6], wren_11_7)
        gate.ANDGate3(wren, dec_1_out[11], dec_2_out[6], wren_12_7)
        gate.ANDGate3(wren, dec_1_out[12], dec_2_out[6], wren_13_7)
        gate.ANDGate3(wren, dec_1_out[13], dec_2_out[6], wren_14_7)
        gate.ANDGate3(wren, dec_1_out[14], dec_2_out[6], wren_15_7)
        gate.ANDGate3(wren, dec_1_out[15], dec_2_out[6], wren_16_7)
        gate.ANDGate3(wren, dec_1_out[0], dec_2_out[7], wren_1_8)
        gate.ANDGate3(wren, dec_1_out[1], dec_2_out[7], wren_2_8)
        gate.ANDGate3(wren, dec_1_out[2], dec_2_out[7], wren_3_8)
        gate.ANDGate3(wren, dec_1_out[3], dec_2_out[7], wren_4_8)
        gate.ANDGate3(wren, dec_1_out[4], dec_2_out[7], wren_5_8)
        gate.ANDGate3(wren, dec_1_out[5], dec_2_out[7], wren_6_8)
        gate.ANDGate3(wren, dec_1_out[6], dec_2_out[7], wren_7_8)
        gate.ANDGate3(wren, dec_1_out[7], dec_2_out[7], wren_8_8)
        gate.ANDGate3(wren, dec_1_out[8], dec_2_out[7], wren_9_8)
        gate.ANDGate3(wren, dec_1_out[9], dec_2_out[7], wren_10_8)
        gate.ANDGate3(wren, dec_1_out[10], dec_2_out[7], wren_11_8)
        gate.ANDGate3(wren, dec_1_out[11], dec_2_out[7], wren_12_8)
        gate.ANDGate3(wren, dec_1_out[12], dec_2_out[7], wren_13_8)
        gate.ANDGate3(wren, dec_1_out[13], dec_2_out[7], wren_14_8)
        gate.ANDGate3(wren, dec_1_out[14], dec_2_out[7], wren_15_8)
        gate.ANDGate3(wren, dec_1_out[15], dec_2_out[7], wren_16_8)
        gate.ANDGate3(wren, dec_1_out[0], dec_2_out[8], wren_1_9)
        gate.ANDGate3(wren, dec_1_out[1], dec_2_out[8], wren_2_9)
        gate.ANDGate3(wren, dec_1_out[2], dec_2_out[8], wren_3_9)
        gate.ANDGate3(wren, dec_1_out[3], dec_2_out[8], wren_4_9)
        gate.ANDGate3(wren, dec_1_out[4], dec_2_out[8], wren_5_9)
        gate.ANDGate3(wren, dec_1_out[5], dec_2_out[8], wren_6_9)
        gate.ANDGate3(wren, dec_1_out[6], dec_2_out[8], wren_7_9)
        gate.ANDGate3(wren, dec_1_out[7], dec_2_out[8], wren_8_9)
        gate.ANDGate3(wren, dec_1_out[8], dec_2_out[8], wren_9_9)
        gate.ANDGate3(wren, dec_1_out[9], dec_2_out[8], wren_10_9)
        gate.ANDGate3(wren, dec_1_out[10], dec_2_out[8], wren_11_9)
        gate.ANDGate3(wren, dec_1_out[11], dec_2_out[8], wren_12_9)
        gate.ANDGate3(wren, dec_1_out[12], dec_2_out[8], wren_13_9)
        gate.ANDGate3(wren, dec_1_out[13], dec_2_out[8], wren_14_9)
        gate.ANDGate3(wren, dec_1_out[14], dec_2_out[8], wren_15_9)
        gate.ANDGate3(wren, dec_1_out[15], dec_2_out[8], wren_16_9)
        gate.ANDGate3(wren, dec_1_out[0], dec_2_out[9], wren_1_10)
        gate.ANDGate3(wren, dec_1_out[1], dec_2_out[9], wren_2_10)
        gate.ANDGate3(wren, dec_1_out[2], dec_2_out[9], wren_3_10)
        gate.ANDGate3(wren, dec_1_out[3], dec_2_out[9], wren_4_10)
        gate.ANDGate3(wren, dec_1_out[4], dec_2_out[9], wren_5_10)
        gate.ANDGate3(wren, dec_1_out[5], dec_2_out[9], wren_6_10)
        gate.ANDGate3(wren, dec_1_out[6], dec_2_out[9], wren_7_10)
        gate.ANDGate3(wren, dec_1_out[7], dec_2_out[9], wren_8_10)
        gate.ANDGate3(wren, dec_1_out[8], dec_2_out[9], wren_9_10)
        gate.ANDGate3(wren, dec_1_out[9], dec_2_out[9], wren_10_10)
        gate.ANDGate3(wren, dec_1_out[10], dec_2_out[9], wren_11_10)
        gate.ANDGate3(wren, dec_1_out[11], dec_2_out[9], wren_12_10)
        gate.ANDGate3(wren, dec_1_out[12], dec_2_out[9], wren_13_10)
        gate.ANDGate3(wren, dec_1_out[13], dec_2_out[9], wren_14_10)
        gate.ANDGate3(wren, dec_1_out[14], dec_2_out[9], wren_15_10)
        gate.ANDGate3(wren, dec_1_out[15], dec_2_out[9], wren_16_10)
        gate.ANDGate3(wren, dec_1_out[0], dec_2_out[10], wren_1_11)
        gate.ANDGate3(wren, dec_1_out[1], dec_2_out[10], wren_2_11)
        gate.ANDGate3(wren, dec_1_out[2], dec_2_out[10], wren_3_11)
        gate.ANDGate3(wren, dec_1_out[3], dec_2_out[10], wren_4_11)
        gate.ANDGate3(wren, dec_1_out[4], dec_2_out[10], wren_5_11)
        gate.ANDGate3(wren, dec_1_out[5], dec_2_out[10], wren_6_11)
        gate.ANDGate3(wren, dec_1_out[6], dec_2_out[10], wren_7_11)
        gate.ANDGate3(wren, dec_1_out[7], dec_2_out[10], wren_8_11)
        gate.ANDGate3(wren, dec_1_out[8], dec_2_out[10], wren_9_11)
        gate.ANDGate3(wren, dec_1_out[9], dec_2_out[10], wren_10_11)
        gate.ANDGate3(wren, dec_1_out[10], dec_2_out[10], wren_11_11)
        gate.ANDGate3(wren, dec_1_out[11], dec_2_out[10], wren_12_11)
        gate.ANDGate3(wren, dec_1_out[12], dec_2_out[10], wren_13_11)
        gate.ANDGate3(wren, dec_1_out[13], dec_2_out[10], wren_14_11)
        gate.ANDGate3(wren, dec_1_out[14], dec_2_out[10], wren_15_11)
        gate.ANDGate3(wren, dec_1_out[15], dec_2_out[10], wren_16_11)
        gate.ANDGate3(wren, dec_1_out[0], dec_2_out[11], wren_1_12)
        gate.ANDGate3(wren, dec_1_out[1], dec_2_out[11], wren_2_12)
        gate.ANDGate3(wren, dec_1_out[2], dec_2_out[11], wren_3_12)
        gate.ANDGate3(wren, dec_1_out[3], dec_2_out[11], wren_4_12)
        gate.ANDGate3(wren, dec_1_out[4], dec_2_out[11], wren_5_12)
        gate.ANDGate3(wren, dec_1_out[5], dec_2_out[11], wren_6_12)
        gate.ANDGate3(wren, dec_1_out[6], dec_2_out[11], wren_7_12)
        gate.ANDGate3(wren, dec_1_out[7], dec_2_out[11], wren_8_12)
        gate.ANDGate3(wren, dec_1_out[8], dec_2_out[11], wren_9_12)
        gate.ANDGate3(wren, dec_1_out[9], dec_2_out[11], wren_10_12)
        gate.ANDGate3(wren, dec_1_out[10], dec_2_out[11], wren_11_12)
        gate.ANDGate3(wren, dec_1_out[11], dec_2_out[11], wren_12_12)
        gate.ANDGate3(wren, dec_1_out[12], dec_2_out[11], wren_13_12)
        gate.ANDGate3(wren, dec_1_out[13], dec_2_out[11], wren_14_12)
        gate.ANDGate3(wren, dec_1_out[14], dec_2_out[11], wren_15_12)
        gate.ANDGate3(wren, dec_1_out[15], dec_2_out[11], wren_16_12)
        gate.ANDGate3(wren, dec_1_out[0], dec_2_out[12], wren_1_13)
        gate.ANDGate3(wren, dec_1_out[1], dec_2_out[12], wren_2_13)
        gate.ANDGate3(wren, dec_1_out[2], dec_2_out[12], wren_3_13)
        gate.ANDGate3(wren, dec_1_out[3], dec_2_out[12], wren_4_13)
        gate.ANDGate3(wren, dec_1_out[4], dec_2_out[12], wren_5_13)
        gate.ANDGate3(wren, dec_1_out[5], dec_2_out[12], wren_6_13)
        gate.ANDGate3(wren, dec_1_out[6], dec_2_out[12], wren_7_13)
        gate.ANDGate3(wren, dec_1_out[7], dec_2_out[12], wren_8_13)
        gate.ANDGate3(wren, dec_1_out[8], dec_2_out[12], wren_9_133)
        gate.ANDGate3(wren, dec_1_out[9], dec_2_out[12], wren_10_13)
        gate.ANDGate3(wren, dec_1_out[10], dec_2_out[12], wren_11_13)
        gate.ANDGate3(wren, dec_1_out[11], dec_2_out[12], wren_12_13)
        gate.ANDGate3(wren, dec_1_out[12], dec_2_out[12], wren_13_13)
        gate.ANDGate3(wren, dec_1_out[13], dec_2_out[12], wren_14_13)
        gate.ANDGate3(wren, dec_1_out[14], dec_2_out[12], wren_15_13)
        gate.ANDGate3(wren, dec_1_out[15], dec_2_out[12], wren_16_13)
        gate.ANDGate3(wren, dec_1_out[0], dec_2_out[13], wren_1_14)
        gate.ANDGate3(wren, dec_1_out[1], dec_2_out[13], wren_2_14)
        gate.ANDGate3(wren, dec_1_out[2], dec_2_out[13], wren_3_14)
        gate.ANDGate3(wren, dec_1_out[3], dec_2_out[13], wren_4_14)
        gate.ANDGate3(wren, dec_1_out[4], dec_2_out[13], wren_5_14)
        gate.ANDGate3(wren, dec_1_out[5], dec_2_out[13], wren_6_14)
        gate.ANDGate3(wren, dec_1_out[6], dec_2_out[13], wren_7_14)
        gate.ANDGate3(wren, dec_1_out[7], dec_2_out[13], wren_8_14)
        gate.ANDGate3(wren, dec_1_out[8], dec_2_out[13], wren_9_14)
        gate.ANDGate3(wren, dec_1_out[9], dec_2_out[13], wren_10_14)
        gate.ANDGate3(wren, dec_1_out[10], dec_2_out[13], wren_11_14)
        gate.ANDGate3(wren, dec_1_out[11], dec_2_out[13], wren_12_14)
        gate.ANDGate3(wren, dec_1_out[12], dec_2_out[13], wren_13_14)
        gate.ANDGate3(wren, dec_1_out[13], dec_2_out[13], wren_14_14)
        gate.ANDGate3(wren, dec_1_out[14], dec_2_out[13], wren_15_14)
        gate.ANDGate3(wren, dec_1_out[15], dec_2_out[13], wren_16_14)
        gate.ANDGate3(wren, dec_1_out[0], dec_2_out[14], wren_1_15)
        gate.ANDGate3(wren, dec_1_out[1], dec_2_out[14], wren_2_15)
        gate.ANDGate3(wren, dec_1_out[2], dec_2_out[14], wren_3_15)
        gate.ANDGate3(wren, dec_1_out[3], dec_2_out[14], wren_4_15)
        gate.ANDGate3(wren, dec_1_out[4], dec_2_out[14], wren_5_15)
        gate.ANDGate3(wren, dec_1_out[5], dec_2_out[14], wren_6_15)
        gate.ANDGate3(wren, dec_1_out[6], dec_2_out[14], wren_7_15)
        gate.ANDGate3(wren, dec_1_out[7], dec_2_out[14], wren_8_15)
        gate.ANDGate3(wren, dec_1_out[8], dec_2_out[14], wren_9_15)
        gate.ANDGate3(wren, dec_1_out[9], dec_2_out[14], wren_10_15)
        gate.ANDGate3(wren, dec_1_out[10], dec_2_out[14], wren_11_15)
        gate.ANDGate3(wren, dec_1_out[11], dec_2_out[14], wren_12_15)
        gate.ANDGate3(wren, dec_1_out[12], dec_2_out[14], wren_13_15)
        gate.ANDGate3(wren, dec_1_out[13], dec_2_out[14], wren_14_15)
        gate.ANDGate3(wren, dec_1_out[14], dec_2_out[14], wren_15_15)
        gate.ANDGate3(wren, dec_1_out[15], dec_2_out[14], wren_16_15)
        gate.ANDGate3(wren, dec_1_out[0], dec_2_out[15], wren_1_16)
        gate.ANDGate3(wren, dec_1_out[1], dec_2_out[15], wren_2_16)
        gate.ANDGate3(wren, dec_1_out[2], dec_2_out[15], wren_3_16)
        gate.ANDGate3(wren, dec_1_out[3], dec_2_out[15], wren_4_16)
        gate.ANDGate3(wren, dec_1_out[4], dec_2_out[15], wren_5_16)
        gate.ANDGate3(wren, dec_1_out[5], dec_2_out[15], wren_6_16)
        gate.ANDGate3(wren, dec_1_out[6], dec_2_out[15], wren_7_16)
        gate.ANDGate3(wren, dec_1_out[7], dec_2_out[15], wren_8_16)
        gate.ANDGate3(wren, dec_1_out[8], dec_2_out[15], wren_9_16)
        gate.ANDGate3(wren, dec_1_out[9], dec_2_out[15], wren_10_16)
        gate.ANDGate3(wren, dec_1_out[10], dec_2_out[15], wren_11_16)
        gate.ANDGate3(wren, dec_1_out[11], dec_2_out[15], wren_12_16)
        gate.ANDGate3(wren, dec_1_out[12], dec_2_out[15], wren_13_16)
        gate.ANDGate3(wren, dec_1_out[13], dec_2_out[15], wren_14_16)
        gate.ANDGate3(wren, dec_1_out[14], dec_2_out[15], wren_15_16)
        gate.ANDGate3(wren, dec_1_out[15], dec_2_out[15], wren_16_16)

        RAM256x4(data_bus, addr_3, wren_1_1, clock, r0)
        RAM256x4(data_bus, addr_3, wren_2_1, clock, r1)
        RAM256x4(data_bus, addr_3, wren_3_1, clock, r2)
        RAM256x4(data_bus, addr_3, wren_4_1, clock, r3)
        RAM256x4(data_bus, addr_3, wren_5_1, clock, r4)
        RAM256x4(data_bus, addr_3, wren_6_1, clock, r5)
        RAM256x4(data_bus, addr_3, wren_7_1, clock, r6)
        RAM256x4(data_bus, addr_3, wren_8_1, clock, r7)
        RAM256x4(data_bus, addr_3, wren_9_1, clock, r8)
        RAM256x4(data_bus, addr_3, wren_10_1, clock, r9)
        RAM256x4(data_bus, addr_3, wren_11_1, clock, r10)
        RAM256x4(data_bus, addr_3, wren_12_1, clock, r11)
        RAM256x4(data_bus, addr_3, wren_13_1, clock, r12)
        RAM256x4(data_bus, addr_3, wren_14_1, clock, r13)
        RAM256x4(data_bus, addr_3, wren_15_1, clock, r14)
        RAM256x4(data_bus, addr_3, wren_16_1, clock, r15)
        RAM256x4(data_bus, addr_3, wren_1_2, clock, r16)
        RAM256x4(data_bus, addr_3, wren_2_2, clock, r17)
        RAM256x4(data_bus, addr_3, wren_3_2, clock, r18)
        RAM256x4(data_bus, addr_3, wren_4_2, clock, r19)
        RAM256x4(data_bus, addr_3, wren_5_2, clock, r20)
        RAM256x4(data_bus, addr_3, wren_6_2, clock, r21)
        RAM256x4(data_bus, addr_3, wren_7_2, clock, r22)
        RAM256x4(data_bus, addr_3, wren_8_2, clock, r23)
        RAM256x4(data_bus, addr_3, wren_9_2, clock, r24)
        RAM256x4(data_bus, addr_3, wren_10_2, clock, r25)
        RAM256x4(data_bus, addr_3, wren_11_2, clock, r26)
        RAM256x4(data_bus, addr_3, wren_12_2, clock, r27)
        RAM256x4(data_bus, addr_3, wren_13_2, clock, r28)
        RAM256x4(data_bus, addr_3, wren_14_2, clock, r29)
        RAM256x4(data_bus, addr_3, wren_15_2, clock, r30)
        RAM256x4(data_bus, addr_3, wren_16_2, clock, r31)
        RAM256x4(data_bus, addr_3, wren_1_3, clock, r32)
        RAM256x4(data_bus, addr_3, wren_2_3, clock, r33)
        RAM256x4(data_bus, addr_3, wren_3_3, clock, r34)
        RAM256x4(data_bus, addr_3, wren_4_3, clock, r35)
        RAM256x4(data_bus, addr_3, wren_5_3, clock, r36)
        RAM256x4(data_bus, addr_3, wren_6_3, clock, r37)
        RAM256x4(data_bus, addr_3, wren_7_3, clock, r38)
        RAM256x4(data_bus, addr_3, wren_8_3, clock, r39)
        RAM256x4(data_bus, addr_3, wren_9_3, clock, r40)
        RAM256x4(data_bus, addr_3, wren_10_3, clock, r41)
        RAM256x4(data_bus, addr_3, wren_11_3, clock, r42)
        RAM256x4(data_bus, addr_3, wren_12_3, clock, r43)
        RAM256x4(data_bus, addr_3, wren_13_3, clock, r44)
        RAM256x4(data_bus, addr_3, wren_14_3, clock, r45)
        RAM256x4(data_bus, addr_3, wren_15_3, clock, r46)
        RAM256x4(data_bus, addr_3, wren_16_3, clock, r47)
        RAM256x4(data_bus, addr_3, wren_1_4, clock, r48)
        RAM256x4(data_bus, addr_3, wren_2_4, clock, r49)
        RAM256x4(data_bus, addr_3, wren_3_4, clock, r50)
        RAM256x4(data_bus, addr_3, wren_4_4, clock, r51)
        RAM256x4(data_bus, addr_3, wren_5_4, clock, r52)
        RAM256x4(data_bus, addr_3, wren_6_4, clock, r53)
        RAM256x4(data_bus, addr_3, wren_7_4, clock, r54)
        RAM256x4(data_bus, addr_3, wren_8_4, clock, r55)
        RAM256x4(data_bus, addr_3, wren_9_4, clock, r56)
        RAM256x4(data_bus, addr_3, wren_10_4, clock, r57)
        RAM256x4(data_bus, addr_3, wren_11_4, clock, r58)
        RAM256x4(data_bus, addr_3, wren_12_4, clock, r59)
        RAM256x4(data_bus, addr_3, wren_13_4, clock, r60)
        RAM256x4(data_bus, addr_3, wren_14_4, clock, r61)
        RAM256x4(data_bus, addr_3, wren_15_4, clock, r62)
        RAM256x4(data_bus, addr_3, wren_16_4, clock, r63)
        RAM256x4(data_bus, addr_3, wren_1_5, clock, r64)
        RAM256x4(data_bus, addr_3, wren_2_5, clock, r65)
        RAM256x4(data_bus, addr_3, wren_3_5, clock, r66)
        RAM256x4(data_bus, addr_3, wren_4_5, clock, r67)
        RAM256x4(data_bus, addr_3, wren_5_5, clock, r68)
        RAM256x4(data_bus, addr_3, wren_6_5, clock, r69)
        RAM256x4(data_bus, addr_3, wren_7_5, clock, r70)
        RAM256x4(data_bus, addr_3, wren_8_5, clock, r71)
        RAM256x4(data_bus, addr_3, wren_9_5, clock, r72)
        RAM256x4(data_bus, addr_3, wren_10_5, clock, r73)
        RAM256x4(data_bus, addr_3, wren_11_5, clock, r74)
        RAM256x4(data_bus, addr_3, wren_12_5, clock, r75)
        RAM256x4(data_bus, addr_3, wren_13_5, clock, r76)
        RAM256x4(data_bus, addr_3, wren_14_5, clock, r77)
        RAM256x4(data_bus, addr_3, wren_15_5, clock, r78)
        RAM256x4(data_bus, addr_3, wren_16_5, clock, r79)
        RAM256x4(data_bus, addr_3, wren_1_6, clock, r80)
        RAM256x4(data_bus, addr_3, wren_2_6, clock, r81)
        RAM256x4(data_bus, addr_3, wren_3_6, clock, r82)
        RAM256x4(data_bus, addr_3, wren_4_6, clock, r83)
        RAM256x4(data_bus, addr_3, wren_5_6, clock, r84)
        RAM256x4(data_bus, addr_3, wren_6_6, clock, r85)
        RAM256x4(data_bus, addr_3, wren_7_6, clock, r86)
        RAM256x4(data_bus, addr_3, wren_8_6, clock, r87)
        RAM256x4(data_bus, addr_3, wren_9_6, clock, r88)
        RAM256x4(data_bus, addr_3, wren_10_6, clock, r89)
        RAM256x4(data_bus, addr_3, wren_11_6, clock, r90)
        RAM256x4(data_bus, addr_3, wren_12_6, clock, r91)
        RAM256x4(data_bus, addr_3, wren_13_6, clock, r92)
        RAM256x4(data_bus, addr_3, wren_14_6, clock, r93)
        RAM256x4(data_bus, addr_3, wren_15_6, clock, r94)
        RAM256x4(data_bus, addr_3, wren_16_6, clock, r95)
        RAM256x4(data_bus, addr_3, wren_1_7, clock, r96)
        RAM256x4(data_bus, addr_3, wren_2_7, clock, r97)
        RAM256x4(data_bus, addr_3, wren_3_7, clock, r98)
        RAM256x4(data_bus, addr_3, wren_4_7, clock, r99)
        RAM256x4(data_bus, addr_3, wren_5_7, clock, r100)
        RAM256x4(data_bus, addr_3, wren_6_7, clock, r101)
        RAM256x4(data_bus, addr_3, wren_7_7, clock, r102)
        RAM256x4(data_bus, addr_3, wren_8_7, clock, r103)
        RAM256x4(data_bus, addr_3, wren_9_7, clock, r104)
        RAM256x4(data_bus, addr_3, wren_10_7, clock, r105)
        RAM256x4(data_bus, addr_3, wren_11_7, clock, r106)
        RAM256x4(data_bus, addr_3, wren_12_7, clock, r107)
        RAM256x4(data_bus, addr_3, wren_13_7, clock, r108)
        RAM256x4(data_bus, addr_3, wren_14_7, clock, r109)
        RAM256x4(data_bus, addr_3, wren_15_7, clock, r110)
        RAM256x4(data_bus, addr_3, wren_16_7, clock, r111)
        RAM256x4(data_bus, addr_3, wren_1_8, clock, r112)
        RAM256x4(data_bus, addr_3, wren_2_8, clock, r113)
        RAM256x4(data_bus, addr_3, wren_3_8, clock, r114)
        RAM256x4(data_bus, addr_3, wren_4_8, clock, r115)
        RAM256x4(data_bus, addr_3, wren_5_8, clock, r116)
        RAM256x4(data_bus, addr_3, wren_6_8, clock, r117)
        RAM256x4(data_bus, addr_3, wren_7_8, clock, r118)
        RAM256x4(data_bus, addr_3, wren_8_8, clock, r119)
        RAM256x4(data_bus, addr_3, wren_9_8, clock, r120)
        RAM256x4(data_bus, addr_3, wren_10_8, clock, r121)
        RAM256x4(data_bus, addr_3, wren_11_8, clock, r122)
        RAM256x4(data_bus, addr_3, wren_12_8, clock, r123)
        RAM256x4(data_bus, addr_3, wren_13_8, clock, r124)
        RAM256x4(data_bus, addr_3, wren_14_8, clock, r125)
        RAM256x4(data_bus, addr_3, wren_15_8, clock, r126)
        RAM256x4(data_bus, addr_3, wren_16_8, clock, r127)
        RAM256x4(data_bus, addr_3, wren_1_9, clock, r128)
        RAM256x4(data_bus, addr_3, wren_2_9, clock, r129)
        RAM256x4(data_bus, addr_3, wren_3_9, clock, r130)
        RAM256x4(data_bus, addr_3, wren_4_9, clock, r131)
        RAM256x4(data_bus, addr_3, wren_5_9, clock, r132)
        RAM256x4(data_bus, addr_3, wren_6_9, clock, r133)
        RAM256x4(data_bus, addr_3, wren_7_9, clock, r134)
        RAM256x4(data_bus, addr_3, wren_8_9, clock, r135)
        RAM256x4(data_bus, addr_3, wren_9_9, clock, r136)
        RAM256x4(data_bus, addr_3, wren_10_9, clock, r137)
        RAM256x4(data_bus, addr_3, wren_11_9, clock, r138)
        RAM256x4(data_bus, addr_3, wren_12_9, clock, r139)
        RAM256x4(data_bus, addr_3, wren_13_9, clock, r140)
        RAM256x4(data_bus, addr_3, wren_14_9, clock, r141)
        RAM256x4(data_bus, addr_3, wren_15_9, clock, r142)
        RAM256x4(data_bus, addr_3, wren_16_9, clock, r143)
        RAM256x4(data_bus, addr_3, wren_1_10, clock, r144)
        RAM256x4(data_bus, addr_3, wren_2_10, clock, r145)
        RAM256x4(data_bus, addr_3, wren_3_10, clock, r146)
        RAM256x4(data_bus, addr_3, wren_4_10, clock, r147)
        RAM256x4(data_bus, addr_3, wren_5_10, clock, r148)
        RAM256x4(data_bus, addr_3, wren_6_10, clock, r149)
        RAM256x4(data_bus, addr_3, wren_7_10, clock, r150)
        RAM256x4(data_bus, addr_3, wren_8_10, clock, r151)
        RAM256x4(data_bus, addr_3, wren_9_10, clock, r152)
        RAM256x4(data_bus, addr_3, wren_10_10, clock, r153)
        RAM256x4(data_bus, addr_3, wren_11_10, clock, r154)
        RAM256x4(data_bus, addr_3, wren_12_10, clock, r155)
        RAM256x4(data_bus, addr_3, wren_13_10, clock, r156)
        RAM256x4(data_bus, addr_3, wren_14_10, clock, r157)
        RAM256x4(data_bus, addr_3, wren_15_10, clock, r158)
        RAM256x4(data_bus, addr_3, wren_16_10, clock, r159)
        RAM256x4(data_bus, addr_3, wren_1_11, clock, r160)
        RAM256x4(data_bus, addr_3, wren_2_11, clock, r161)
        RAM256x4(data_bus, addr_3, wren_3_11, clock, r162)
        RAM256x4(data_bus, addr_3, wren_4_11, clock, r163)
        RAM256x4(data_bus, addr_3, wren_5_11, clock, r164)
        RAM256x4(data_bus, addr_3, wren_6_11, clock, r165)
        RAM256x4(data_bus, addr_3, wren_7_11, clock, r166)
        RAM256x4(data_bus, addr_3, wren_8_11, clock, r167)
        RAM256x4(data_bus, addr_3, wren_9_11, clock, r168)
        RAM256x4(data_bus, addr_3, wren_10_11, clock, r169)
        RAM256x4(data_bus, addr_3, wren_11_11, clock, r170)
        RAM256x4(data_bus, addr_3, wren_12_11, clock, r171)
        RAM256x4(data_bus, addr_3, wren_13_11, clock, r172)
        RAM256x4(data_bus, addr_3, wren_14_11, clock, r173)
        RAM256x4(data_bus, addr_3, wren_15_11, clock, r174)
        RAM256x4(data_bus, addr_3, wren_16_11, clock, r175)
        RAM256x4(data_bus, addr_3, wren_1_12, clock, r176)
        RAM256x4(data_bus, addr_3, wren_2_12, clock, r177)
        RAM256x4(data_bus, addr_3, wren_3_12, clock, r178)
        RAM256x4(data_bus, addr_3, wren_4_12, clock, r179)
        RAM256x4(data_bus, addr_3, wren_5_12, clock, r180)
        RAM256x4(data_bus, addr_3, wren_6_12, clock, r181)
        RAM256x4(data_bus, addr_3, wren_7_12, clock, r182)
        RAM256x4(data_bus, addr_3, wren_8_12, clock, r183)
        RAM256x4(data_bus, addr_3, wren_9_12, clock, r184)
        RAM256x4(data_bus, addr_3, wren_10_12, clock, r185)
        RAM256x4(data_bus, addr_3, wren_11_12, clock, r186)
        RAM256x4(data_bus, addr_3, wren_12_12, clock, r187)
        RAM256x4(data_bus, addr_3, wren_13_12, clock, r188)
        RAM256x4(data_bus, addr_3, wren_14_12, clock, r189)
        RAM256x4(data_bus, addr_3, wren_15_12, clock, r190)
        RAM256x4(data_bus, addr_3, wren_16_12, clock, r191)
        RAM256x4(data_bus, addr_3, wren_1_13, clock, r192)
        RAM256x4(data_bus, addr_3, wren_2_13, clock, r193)
        RAM256x4(data_bus, addr_3, wren_3_13, clock, r194)
        RAM256x4(data_bus, addr_3, wren_4_13, clock, r195)
        RAM256x4(data_bus, addr_3, wren_5_13, clock, r196)
        RAM256x4(data_bus, addr_3, wren_6_13, clock, r197)
        RAM256x4(data_bus, addr_3, wren_7_13, clock, r198)
        RAM256x4(data_bus, addr_3, wren_8_13, clock, r199)
        RAM256x4(data_bus, addr_3, wren_9_13, clock, r200)
        RAM256x4(data_bus, addr_3, wren_10_13, clock, r201)
        RAM256x4(data_bus, addr_3, wren_11_13, clock, r202)
        RAM256x4(data_bus, addr_3, wren_12_13, clock, r203)
        RAM256x4(data_bus, addr_3, wren_13_13, clock, r204)
        RAM256x4(data_bus, addr_3, wren_14_13, clock, r205)
        RAM256x4(data_bus, addr_3, wren_15_13, clock, r206)
        RAM256x4(data_bus, addr_3, wren_16_13, clock, r207)
        RAM256x4(data_bus, addr_3, wren_1_14, clock, r208)
        RAM256x4(data_bus, addr_3, wren_2_14, clock, r209)
        RAM256x4(data_bus, addr_3, wren_3_14, clock, r210)
        RAM256x4(data_bus, addr_3, wren_4_14, clock, r211)
        RAM256x4(data_bus, addr_3, wren_5_14, clock, r212)
        RAM256x4(data_bus, addr_3, wren_6_14, clock, r213)
        RAM256x4(data_bus, addr_3, wren_7_14, clock, r214)
        RAM256x4(data_bus, addr_3, wren_8_14, clock, r215)
        RAM256x4(data_bus, addr_3, wren_9_14, clock, r216)
        RAM256x4(data_bus, addr_3, wren_10_14, clock, r217)
        RAM256x4(data_bus, addr_3, wren_11_14, clock, r218)
        RAM256x4(data_bus, addr_3, wren_12_14, clock, r219)
        RAM256x4(data_bus, addr_3, wren_13_14, clock, r220)
        RAM256x4(data_bus, addr_3, wren_14_14, clock, r221)
        RAM256x4(data_bus, addr_3, wren_15_14, clock, r222)
        RAM256x4(data_bus, addr_3, wren_16_14, clock, r223)
        RAM256x4(data_bus, addr_3, wren_1_15, clock, r224)
        RAM256x4(data_bus, addr_3, wren_2_15, clock, r225)
        RAM256x4(data_bus, addr_3, wren_3_15, clock, r226)
        RAM256x4(data_bus, addr_3, wren_4_15, clock, r227)
        RAM256x4(data_bus, addr_3, wren_5_15, clock, r228)
        RAM256x4(data_bus, addr_3, wren_6_15, clock, r229)
        RAM256x4(data_bus, addr_3, wren_7_15, clock, r230)
        RAM256x4(data_bus, addr_3, wren_8_15, clock, r231)
        RAM256x4(data_bus, addr_3, wren_9_15, clock, r232)
        RAM256x4(data_bus, addr_3, wren_10_15, clock, r233)
        RAM256x4(data_bus, addr_3, wren_11_15, clock, r234)
        RAM256x4(data_bus, addr_3, wren_12_15, clock, r235)
        RAM256x4(data_bus, addr_3, wren_13_15, clock, r236)
        RAM256x4(data_bus, addr_3, wren_14_15, clock, r237)
        RAM256x4(data_bus, addr_3, wren_15_15, clock, r238)
        RAM256x4(data_bus, addr_3, wren_16_15, clock, r239)
        RAM256x4(data_bus, addr_3, wren_1_16, clock, r240)
        RAM256x4(data_bus, addr_3, wren_2_16, clock, r241)
        RAM256x4(data_bus, addr_3, wren_3_16, clock, r242)
        RAM256x4(data_bus, addr_3, wren_4_16, clock, r243)
        RAM256x4(data_bus, addr_3, wren_5_16, clock, r244)
        RAM256x4(data_bus, addr_3, wren_6_16, clock, r245)
        RAM256x4(data_bus, addr_3, wren_7_16, clock, r246)
        RAM256x4(data_bus, addr_3, wren_8_16, clock, r247)
        RAM256x4(data_bus, addr_3, wren_9_16, clock, r248)
        RAM256x4(data_bus, addr_3, wren_10_16, clock, r249)
        RAM256x4(data_bus, addr_3, wren_11_16, clock, r250)
        RAM256x4(data_bus, addr_3, wren_12_16, clock, r251)
        RAM256x4(data_bus, addr_3, wren_13_16, clock, r252)
        RAM256x4(data_bus, addr_3, wren_14_16, clock, r253)
        RAM256x4(data_bus, addr_3, wren_15_16, clock, r254)
        RAM256x4(data_bus, addr_3, wren_16_16, clock, r255)

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
