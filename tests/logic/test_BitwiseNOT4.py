import bitwise as bw


class TestBitwiseNOT4:
    def test_BitwiseNOT4(self):
        a_bus_1 = bw.wire.Wire()
        a_bus_2 = bw.wire.Wire()
        a_bus_3 = bw.wire.Wire()
        a_bus_4 = bw.wire.Wire()
        a_bus = bw.wire.Bus4(a_bus_1, a_bus_2, a_bus_3, a_bus_4)

        o_bus_1 = bw.wire.Wire()
        o_bus_2 = bw.wire.Wire()
        o_bus_3 = bw.wire.Wire()
        o_bus_4 = bw.wire.Wire()
        o_bus = bw.wire.Bus4(o_bus_1, o_bus_2, o_bus_3, o_bus_4)

        a = bw.logic.BitwiseNOT4(a_bus, o_bus)

        assert o_bus.wire_values == (1, 1, 1, 1)

        a_bus[0].value = 0
        a_bus[1].value = 1
        a_bus[2].value = 0
        a_bus[3].value = 1

        assert o_bus.wire_values == (1, 0, 1, 0)

        a_bus[0].value = 0
        a_bus[1].value = 0
        a_bus[2].value = 1
        a_bus[3].value = 1

        assert o_bus.wire_values == (1, 1, 0, 0)

        a_bus[0].value = 1
        a_bus[1].value = 1
        a_bus[2].value = 1
        a_bus[3].value = 1

        assert o_bus.wire_values == (0, 0, 0, 0)

        print(a.__doc__)
        print(a)

        a(
            input_bus=(0, 0, 1, 1),
            output_bus=None
        )
        assert o_bus.wire_values == (1, 1, 0, 0)
