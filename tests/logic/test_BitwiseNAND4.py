import bitwise as bw


class TestBitwiseNAND4:
    def test_BitwiseNAND4(self):
        a_bus_1 = bw.wire.Wire()
        a_bus_2 = bw.wire.Wire()
        a_bus_3 = bw.wire.Wire()
        a_bus_4 = bw.wire.Wire()
        a_bus = bw.wire.Bus4(a_bus_1, a_bus_2, a_bus_3, a_bus_4)

        b_bus_1 = bw.wire.Wire()
        b_bus_2 = bw.wire.Wire()
        b_bus_3 = bw.wire.Wire()
        b_bus_4 = bw.wire.Wire()
        b_bus = bw.wire.Bus4(b_bus_1, b_bus_2, b_bus_3, b_bus_4)

        o_bus_1 = bw.wire.Wire()
        o_bus_2 = bw.wire.Wire()
        o_bus_3 = bw.wire.Wire()
        o_bus_4 = bw.wire.Wire()
        o_bus = bw.wire.Bus4(o_bus_1, o_bus_2, o_bus_3, o_bus_4)

        bw.logic.BitwiseNAND4(a_bus, b_bus, o_bus)

        assert o_bus.wire_values == (1, 1, 1, 1)

        a_bus[0].value = 1
        a_bus[1].value = 1
        a_bus[2].value = 1
        a_bus[3].value = 1

        b_bus[0].value = 0
        b_bus[1].value = 1
        b_bus[2].value = 0
        b_bus[3].value = 1

        assert o_bus.wire_values == (1, 0, 1, 0)

        b_bus[0].value = 0
        b_bus[1].value = 0
        b_bus[2].value = 1
        b_bus[3].value = 1

        assert o_bus.wire_values == (1, 1, 0, 0)

        b_bus[0].value = 1
        b_bus[1].value = 1
        b_bus[2].value = 1
        b_bus[3].value = 1

        assert o_bus.wire_values == (0, 0, 0, 0)
