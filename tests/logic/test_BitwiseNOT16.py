import bitwise as bw


class TestBitwiseNOT16:
    def test_BitwiseNOT16(self):
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

        bw.logic.BitwiseNOT16(a_bus, o_bus)

        assert o_bus.wire_values == (
            1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1)

        a_bus[0].value = 0
        a_bus[1].value = 1
        a_bus[2].value = 0
        a_bus[3].value = 1
        a_bus[4].value = 0
        a_bus[5].value = 1
        a_bus[6].value = 0
        a_bus[7].value = 1
        a_bus[8].value = 0
        a_bus[9].value = 1
        a_bus[10].value = 0
        a_bus[11].value = 1
        a_bus[12].value = 0
        a_bus[13].value = 1
        a_bus[14].value = 0
        a_bus[15].value = 1

        assert o_bus.wire_values == (
            1, 0, 1, 0, 1, 0, 1, 0, 1, 0, 1, 0, 1, 0, 1, 0)

        a_bus[0].value = 0
        a_bus[1].value = 0
        a_bus[2].value = 0
        a_bus[3].value = 0
        a_bus[4].value = 0
        a_bus[5].value = 0
        a_bus[6].value = 0
        a_bus[7].value = 0
        a_bus[8].value = 1
        a_bus[9].value = 1
        a_bus[10].value = 1
        a_bus[11].value = 1
        a_bus[12].value = 1
        a_bus[13].value = 1
        a_bus[14].value = 1
        a_bus[15].value = 1

        assert o_bus.wire_values == (
            1, 1, 1, 1, 1, 1, 1, 1, 0, 0, 0, 0, 0, 0, 0, 0)

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

        assert o_bus.wire_values == (
            0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0)
