import bitwise as bw


class TestDownCounterMod16:
    def test_DownCounterMod16(self):
        enable = bw.wire.Wire()
        load_n = bw.wire.Wire()
        clock = bw.wire.Wire()
        load_1 = bw.wire.Wire()
        load_2 = bw.wire.Wire()
        load_3 = bw.wire.Wire()
        load_4 = bw.wire.Wire()
        output_1 = bw.wire.Wire()
        output_2 = bw.wire.Wire()
        output_3 = bw.wire.Wire()
        output_4 = bw.wire.Wire()
        load_bus = bw.wire.Bus4(load_1, load_2, load_3, load_4)
        output_bus = bw.wire.Bus4(output_1, output_2, output_3, output_4)

        a = bw.state.DownCounterMod16(
            enable,
            load_n,
            load_bus,
            clock,
            output_bus
        )

        enable.value = 1

        load_n.value = 0
        load_1.value = 1
        load_2.value = 1
        load_3.value = 1
        load_4.value = 1
        clock.value = 0
        clock.value = 1
        load_n.value = 1
        assert output_bus.wire_values == (1, 1, 1, 1)

        clock.value = 0
        clock.value = 1
        assert output_bus.wire_values == (1, 1, 1, 0)

        clock.value = 0
        clock.value = 1
        assert output_bus.wire_values == (1, 1, 0, 1)

        clock.value = 0
        clock.value = 1
        assert output_bus.wire_values == (1, 1, 0, 0)

        clock.value = 0
        clock.value = 1
        assert output_bus.wire_values == (1, 0, 1, 1)

        clock.value = 0
        clock.value = 1
        assert output_bus.wire_values == (1, 0, 1, 0)

        clock.value = 0
        clock.value = 1
        assert output_bus.wire_values == (1, 0, 0, 1)

        clock.value = 0
        clock.value = 1
        assert output_bus.wire_values == (1, 0, 0, 0)

        clock.value = 0
        clock.value = 1
        assert output_bus.wire_values == (0, 1, 1, 1)

        clock.value = 0
        clock.value = 1
        assert output_bus.wire_values == (0, 1, 1, 0)

        clock.value = 0
        clock.value = 1
        assert output_bus.wire_values == (0, 1, 0, 1)

        clock.value = 0
        clock.value = 1
        assert output_bus.wire_values == (0, 1, 0, 0)

        clock.value = 0
        clock.value = 1
        assert output_bus.wire_values == (0, 0, 1, 1)

        clock.value = 0
        clock.value = 1
        assert output_bus.wire_values == (0, 0, 1, 0)

        clock.value = 0
        clock.value = 1
        assert output_bus.wire_values == (0, 0, 0, 1)

        clock.value = 0
        clock.value = 1
        assert output_bus.wire_values == (0, 0, 0, 0)

        clock.value = 0
        clock.value = 1
        assert output_bus.wire_values == (1, 1, 1, 1)

        clock.value = 0
        clock.value = 1
        assert output_bus.wire_values == (1, 1, 1, 0)

        enable.value = 0

        clock.value = 0
        clock.value = 1
        assert output_bus.wire_values == (1, 1, 1, 0)

        enable.value = 1

        clock.value = 0
        clock.value = 1
        assert output_bus.wire_values == (1, 1, 0, 1)

        load_n.value = 0
        load_1.value = 0
        load_2.value = 0
        load_3.value = 1
        load_4.value = 0
        clock.value = 0
        clock.value = 1
        load_n.value = 1
        assert output_bus.wire_values == (0, 0, 1, 0)

        print(a.__doc__)
        print(a)
