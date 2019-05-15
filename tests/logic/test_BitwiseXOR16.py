import bitwise as bw


class TestBitwiseXOR16:
    def test_BitwiseXOR16(self):
        a_bus_1 = bw.wire.Wire()
        a_bus_2 = bw.wire.Wire()
        a_bus_3 = bw.wire.Wire()
        a_bus_4 = bw.wire.Wire()
        a_bus_5 = bw.wire.Wire()
        a_bus_6 = bw.wire.Wire()
        a_bus_7 = bw.wire.Wire()
        a_bus_8 = bw.wire.Wire()
        a_bus_9 = bw.wire.Wire()
        a_bus_10 = bw.wire.Wire()
        a_bus_11 = bw.wire.Wire()
        a_bus_12 = bw.wire.Wire()
        a_bus_13 = bw.wire.Wire()
        a_bus_14 = bw.wire.Wire()
        a_bus_15 = bw.wire.Wire()
        a_bus_16 = bw.wire.Wire()
        a_bus = bw.wire.Bus16(
            a_bus_1,
            a_bus_2,
            a_bus_3,
            a_bus_4,
            a_bus_5,
            a_bus_6,
            a_bus_7,
            a_bus_8,
            a_bus_9,
            a_bus_10,
            a_bus_11,
            a_bus_12,
            a_bus_13,
            a_bus_14,
            a_bus_15,
            a_bus_16
        )

        b_bus_1 = bw.wire.Wire()
        b_bus_2 = bw.wire.Wire()
        b_bus_3 = bw.wire.Wire()
        b_bus_4 = bw.wire.Wire()
        b_bus_5 = bw.wire.Wire()
        b_bus_6 = bw.wire.Wire()
        b_bus_7 = bw.wire.Wire()
        b_bus_8 = bw.wire.Wire()
        b_bus_9 = bw.wire.Wire()
        b_bus_10 = bw.wire.Wire()
        b_bus_11 = bw.wire.Wire()
        b_bus_12 = bw.wire.Wire()
        b_bus_13 = bw.wire.Wire()
        b_bus_14 = bw.wire.Wire()
        b_bus_15 = bw.wire.Wire()
        b_bus_16 = bw.wire.Wire()
        b_bus = bw.wire.Bus16(
            b_bus_1,
            b_bus_2,
            b_bus_3,
            b_bus_4,
            b_bus_5,
            b_bus_6,
            b_bus_7,
            b_bus_8,
            b_bus_9,
            b_bus_10,
            b_bus_11,
            b_bus_12,
            b_bus_13,
            b_bus_14,
            b_bus_15,
            b_bus_16
        )

        o_bus_1 = bw.wire.Wire()
        o_bus_2 = bw.wire.Wire()
        o_bus_3 = bw.wire.Wire()
        o_bus_4 = bw.wire.Wire()
        o_bus_5 = bw.wire.Wire()
        o_bus_6 = bw.wire.Wire()
        o_bus_7 = bw.wire.Wire()
        o_bus_8 = bw.wire.Wire()
        o_bus_9 = bw.wire.Wire()
        o_bus_10 = bw.wire.Wire()
        o_bus_11 = bw.wire.Wire()
        o_bus_12 = bw.wire.Wire()
        o_bus_13 = bw.wire.Wire()
        o_bus_14 = bw.wire.Wire()
        o_bus_15 = bw.wire.Wire()
        o_bus_16 = bw.wire.Wire()
        o_bus = bw.wire.Bus16(
            o_bus_1,
            o_bus_2,
            o_bus_3,
            o_bus_4,
            o_bus_5,
            o_bus_6,
            o_bus_7,
            o_bus_8,
            o_bus_9,
            o_bus_10,
            o_bus_11,
            o_bus_12,
            o_bus_13,
            o_bus_14,
            o_bus_15,
            o_bus_16
        )

        a = bw.logic.BitwiseXOR16(a_bus, b_bus, o_bus)

        assert o_bus.wire_values == (
            0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0)

        b_bus[0].value = 0
        b_bus[1].value = 1
        b_bus[2].value = 0
        b_bus[3].value = 1
        b_bus[4].value = 0
        b_bus[5].value = 1
        b_bus[6].value = 0
        b_bus[7].value = 1
        b_bus[8].value = 0
        b_bus[9].value = 1
        b_bus[10].value = 0
        b_bus[11].value = 1
        b_bus[12].value = 0
        b_bus[13].value = 1
        b_bus[14].value = 0
        b_bus[15].value = 1

        assert o_bus.wire_values == (
            0, 1, 0, 1, 0, 1, 0, 1, 0, 1, 0, 1, 0, 1, 0, 1)

        b_bus[0].value = 0
        b_bus[1].value = 0
        b_bus[2].value = 0
        b_bus[3].value = 0
        b_bus[4].value = 0
        b_bus[5].value = 0
        b_bus[6].value = 0
        b_bus[7].value = 0
        b_bus[8].value = 1
        b_bus[9].value = 1
        b_bus[10].value = 1
        b_bus[11].value = 1
        b_bus[12].value = 1
        b_bus[13].value = 1
        b_bus[14].value = 1
        b_bus[15].value = 1

        assert o_bus.wire_values == (
            0, 0, 0, 0, 0, 0, 0, 0, 1, 1, 1, 1, 1, 1, 1, 1)

        a_bus[0].value = 1
        a_bus[1].value = 1
        a_bus[2].value = 1
        a_bus[3].value = 1
        a_bus[4].value = 1
        a_bus[5].value = 1
        a_bus[6].value = 1
        a_bus[7].value = 1
        a_bus[8].value = 1
        a_bus[9].value = 1
        a_bus[10].value = 1
        a_bus[11].value = 1
        a_bus[12].value = 1
        a_bus[13].value = 1
        a_bus[14].value = 1
        a_bus[15].value = 1

        b_bus[0].value = 1
        b_bus[1].value = 1
        b_bus[2].value = 1
        b_bus[3].value = 1
        b_bus[4].value = 1
        b_bus[5].value = 1
        b_bus[6].value = 1
        b_bus[7].value = 1
        b_bus[8].value = 1
        b_bus[9].value = 1
        b_bus[10].value = 1
        b_bus[11].value = 1
        b_bus[12].value = 1
        b_bus[13].value = 1
        b_bus[14].value = 1
        b_bus[15].value = 1

        assert o_bus.wire_values == (
            0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0)

        print(a.__doc__)
        print(a)

        a(
            a_bus=(0, 0, 0, 0, 0, 0, 0, 0, 1, 1, 1, 1, 1, 1, 1, 1),
            b_bus=(0, 1, 0, 1, 0, 1, 0, 1, 0, 1, 0, 1, 0, 1, 0, 1),
            output_bus=None
        )
        assert o_bus.wire_values == (
            0, 1, 0, 1, 0, 1, 0, 1, 1, 0, 1, 0, 1, 0, 1, 0)
