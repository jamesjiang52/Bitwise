import bitwise as bw


class TestRingCounter4:
    def test_RingCounter4(self):
        enable = bw.wire.Wire()
        clear_n = bw.wire.Wire()
        clock = bw.wire.Wire()
        output_1 = bw.wire.Wire()
        output_2 = bw.wire.Wire()
        output_3 = bw.wire.Wire()
        output_4 = bw.wire.Wire()
        output_bus = bw.wire.Bus4(output_1, output_2, output_3, output_4)

        a = bw.state.RingCounter4(enable, clear_n, clock, output_bus)

        enable.value = 1
        clear_n.value = 0
        clear_n.value = 1
        assert output_bus.wire_values == (0, 0, 0, 1)

        clock.value = 0
        clock.value = 1
        assert output_bus.wire_values == (0, 0, 1, 0)

        clock.value = 0
        clock.value = 1
        assert output_bus.wire_values == (0, 1, 0, 0)

        clock.value = 0
        clock.value = 1
        assert output_bus.wire_values == (1, 0, 0, 0)

        clock.value = 0
        clock.value = 1
        assert output_bus.wire_values == (0, 0, 0, 1)

        clock.value = 0
        clock.value = 1
        assert output_bus.wire_values == (0, 0, 1, 0)

        enable.value = 0
        clock.value = 0
        clock.value = 1
        assert output_bus.wire_values == (0, 0, 1, 0)

        enable.value = 1
        clock.value = 0
        clock.value = 1
        assert output_bus.wire_values == (0, 1, 0, 0)

        clear_n.value = 0
        assert output_bus.wire_values == (0, 0, 0, 1)

        print(a.__doc__)
        print(a)

        a(
            enable=1,
            clear_n=1,
            clock=0,
            output_bus=None
        )
        assert output_bus.wire_values == (0, 0, 0, 1)
        a(clock=1)
        assert output_bus.wire_values == (0, 0, 1, 0)
