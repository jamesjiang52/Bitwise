import bitwise as bw


class TestRingCounter8:
    def test_RingCounter8(self):
        enable = bw.wire.Wire()
        clear_n = bw.wire.Wire()
        clock = bw.wire.Wire()
        output_1 = bw.wire.Wire()
        output_2 = bw.wire.Wire()
        output_3 = bw.wire.Wire()
        output_4 = bw.wire.Wire()
        output_5 = bw.wire.Wire()
        output_6 = bw.wire.Wire()
        output_7 = bw.wire.Wire()
        output_8 = bw.wire.Wire()
        output_bus = bw.wire.Bus8(
            output_1,
            output_2,
            output_3,
            output_4,
            output_5,
            output_6,
            output_7,
            output_8
        )

        a = bw.state.RingCounter8(enable, clear_n, clock, output_bus)

        enable.value = 1
        clear_n.value = 0
        clear_n.value = 1
        assert output_bus.wire_values == (0, 0, 0, 0, 0, 0, 0, 1)

        clock.value = 0
        clock.value = 1
        assert output_bus.wire_values == (0, 0, 0, 0, 0, 0, 1, 0)

        clock.value = 0
        clock.value = 1
        assert output_bus.wire_values == (0, 0, 0, 0, 0, 1, 0, 0)

        clock.value = 0
        clock.value = 1
        assert output_bus.wire_values == (0, 0, 0, 0, 1, 0, 0, 0)

        clock.value = 0
        clock.value = 1
        assert output_bus.wire_values == (0, 0, 0, 1, 0, 0, 0, 0)

        clock.value = 0
        clock.value = 1
        assert output_bus.wire_values == (0, 0, 1, 0, 0, 0, 0, 0)

        clock.value = 0
        clock.value = 1
        assert output_bus.wire_values == (0, 1, 0, 0, 0, 0, 0, 0)

        clock.value = 0
        clock.value = 1
        assert output_bus.wire_values == (1, 0, 0, 0, 0, 0, 0, 0)

        clock.value = 0
        clock.value = 1
        assert output_bus.wire_values == (0, 0, 0, 0, 0, 0, 0, 1)

        clock.value = 0
        clock.value = 1
        assert output_bus.wire_values == (0, 0, 0, 0, 0, 0, 1, 0)

        enable.value = 0
        clock.value = 0
        clock.value = 1
        assert output_bus.wire_values == (0, 0, 0, 0, 0, 0, 1, 0)

        enable.value = 1
        clock.value = 0
        clock.value = 1
        assert output_bus.wire_values == (0, 0, 0, 0, 0, 1, 0, 0)

        clear_n.value = 0
        assert output_bus.wire_values == (0, 0, 0, 0, 0, 0, 0, 1)

        print(a.__doc__)
        print(a)
