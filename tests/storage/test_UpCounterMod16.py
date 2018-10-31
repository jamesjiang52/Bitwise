import bitwise as bw


class TestUpCounterMod16:
    def test_UpCounterMod16(self):
        enable = bw.wire.Wire()
        clear_n = bw.wire.Wire()
        clock = bw.wire.Wire()
        output_1 = bw.wire.Wire()
        output_2 = bw.wire.Wire()
        output_3 = bw.wire.Wire()
        output_4 = bw.wire.Wire()
        output_bus = bw.wire.Bus4(output_1, output_2, output_3, output_4)

        bw.storage.UpCounterMod16(
            enable,
            clear_n,
            clock,
            output_bus
        )

        clear_n.value = 0
        clear_n.value = 1
        assert output_bus.wire_values == (0, 0, 0, 0)

        enable.value = 1

        clock.value = 0
        clock.value = 1
        assert output_bus.wire_values == (0, 0, 0, 1)

        clock.value = 0
        clock.value = 1
        assert output_bus.wire_values == (0, 0, 1, 0)

        clock.value = 0
        clock.value = 1
        assert output_bus.wire_values == (0, 0, 1, 1)

        clock.value = 0
        clock.value = 1
        assert output_bus.wire_values == (0, 1, 0, 0)

        clock.value = 0
        clock.value = 1
        assert output_bus.wire_values == (0, 1, 0, 1)

        clock.value = 0
        clock.value = 1
        assert output_bus.wire_values == (0, 1, 1, 0)

        clock.value = 0
        clock.value = 1
        assert output_bus.wire_values == (0, 1, 1, 1)

        clock.value = 0
        clock.value = 1
        assert output_bus.wire_values == (1, 0, 0, 0)

        clock.value = 0
        clock.value = 1
        assert output_bus.wire_values == (1, 0, 0, 1)

        clock.value = 0
        clock.value = 1
        assert output_bus.wire_values == (1, 0, 1, 0)

        clock.value = 0
        clock.value = 1
        assert output_bus.wire_values == (1, 0, 1, 1)

        clock.value = 0
        clock.value = 1
        assert output_bus.wire_values == (1, 1, 0, 0)

        clock.value = 0
        clock.value = 1
        assert output_bus.wire_values == (1, 1, 0, 1)

        clock.value = 0
        clock.value = 1
        assert output_bus.wire_values == (1, 1, 1, 0)

        clock.value = 0
        clock.value = 1
        assert output_bus.wire_values == (1, 1, 1, 1)

        clock.value = 0
        clock.value = 1
        assert output_bus.wire_values == (0, 0, 0, 0)

        clock.value = 0
        clock.value = 1
        assert output_bus.wire_values == (0, 0, 0, 1)

        enable.value = 0

        clock.value = 0
        clock.value = 1
        assert output_bus.wire_values == (0, 0, 0, 1)

        enable.value = 1

        clock.value = 0
        clock.value = 1
        assert output_bus.wire_values == (0, 0, 1, 0)

        clear_n.value = 0
        assert output_bus.wire_values == (0, 0, 0, 0)
