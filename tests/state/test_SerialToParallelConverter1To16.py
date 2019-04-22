import bitwise as bw


class TestSerialToParallelConverter1To16:
    def test_SerialToParallelConverter1To16(self):
        enable = bw.wire.Wire()
        reset_n = bw.wire.Wire()
        data = bw.wire.Wire()
        clock = bw.wire.Wire()
        output_1 = bw.wire.Wire()
        output_2 = bw.wire.Wire()
        output_3 = bw.wire.Wire()
        output_4 = bw.wire.Wire()
        output_5 = bw.wire.Wire()
        output_6 = bw.wire.Wire()
        output_7 = bw.wire.Wire()
        output_8 = bw.wire.Wire()
        output_9 = bw.wire.Wire()
        output_10 = bw.wire.Wire()
        output_11 = bw.wire.Wire()
        output_12 = bw.wire.Wire()
        output_13 = bw.wire.Wire()
        output_14 = bw.wire.Wire()
        output_15 = bw.wire.Wire()
        output_16 = bw.wire.Wire()
        output_bus = bw.wire.Bus16(
            output_1,
            output_2,
            output_3,
            output_4,
            output_5,
            output_6,
            output_7,
            output_8,
            output_9,
            output_10,
            output_11,
            output_12,
            output_13,
            output_14,
            output_15,
            output_16
        )

        a = bw.state.SerialToParallelConverter1To16(
            enable,
            reset_n,
            data,
            clock,
            output_bus
        )

        enable.value = 1

        reset_n.value = 0
        assert output_bus.wire_values == (
            0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0)
        reset_n.value = 1

        clock.value = 0
        data.value = 1
        clock.value = 1
        assert output_bus.wire_values == (
            1, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0)

        clock.value = 0
        data.value = 1
        clock.value = 1
        assert output_bus.wire_values == (
            1, 1, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0)

        clock.value = 0
        data.value = 1
        clock.value = 1
        assert output_bus.wire_values == (
            1, 1, 1, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0)

        clock.value = 0
        data.value = 1
        clock.value = 1
        assert output_bus.wire_values == (
            1, 1, 1, 1, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0)

        clock.value = 0
        data.value = 1
        clock.value = 1
        assert output_bus.wire_values == (
            1, 1, 1, 1, 1, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0)

        clock.value = 0
        data.value = 1
        clock.value = 1
        assert output_bus.wire_values == (
            1, 1, 1, 1, 1, 1, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0)

        clock.value = 0
        data.value = 1
        clock.value = 1
        assert output_bus.wire_values == (
            1, 1, 1, 1, 1, 1, 1, 0, 0, 0, 0, 0, 0, 0, 0, 0)

        clock.value = 0
        data.value = 1
        clock.value = 1
        assert output_bus.wire_values == (
            1, 1, 1, 1, 1, 1, 1, 1, 0, 0, 0, 0, 0, 0, 0, 0)

        clock.value = 0
        data.value = 1
        clock.value = 1
        assert output_bus.wire_values == (
            1, 1, 1, 1, 1, 1, 1, 1, 1, 0, 0, 0, 0, 0, 0, 0)

        clock.value = 0
        data.value = 1
        clock.value = 1
        assert output_bus.wire_values == (
            1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 0, 0, 0, 0, 0, 0)

        clock.value = 0
        data.value = 1
        clock.value = 1
        assert output_bus.wire_values == (
            1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 0, 0, 0, 0, 0)

        clock.value = 0
        data.value = 1
        clock.value = 1
        assert output_bus.wire_values == (
            1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 0, 0, 0, 0)

        clock.value = 0
        data.value = 1
        clock.value = 1
        assert output_bus.wire_values == (
            1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 0, 0, 0)

        clock.value = 0
        data.value = 1
        clock.value = 1
        assert output_bus.wire_values == (
            1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 0, 0)

        clock.value = 0
        data.value = 1
        clock.value = 1
        assert output_bus.wire_values == (
            1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 0)

        clock.value = 0
        data.value = 1
        clock.value = 1
        assert output_bus.wire_values == (
            1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1)

        clock.value = 0
        data.value = 0
        clock.value = 1
        assert output_bus.wire_values == (
            0, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1)

        clock.value = 0
        data.value = 0
        clock.value = 1
        assert output_bus.wire_values == (
            0, 0, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1)

        clock.value = 0
        data.value = 0
        clock.value = 1
        assert output_bus.wire_values == (
            0, 0, 0, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1)

        clock.value = 0
        data.value = 0
        clock.value = 1
        assert output_bus.wire_values == (
            0, 0, 0, 0, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1)

        clock.value = 0
        data.value = 0
        clock.value = 1
        assert output_bus.wire_values == (
            0, 0, 0, 0, 0, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1)

        clock.value = 0
        data.value = 0
        clock.value = 1
        assert output_bus.wire_values == (
            0, 0, 0, 0, 0, 0, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1)

        clock.value = 0
        data.value = 0
        clock.value = 1
        assert output_bus.wire_values == (
            0, 0, 0, 0, 0, 0, 0, 1, 1, 1, 1, 1, 1, 1, 1, 1)

        clock.value = 0
        data.value = 0
        clock.value = 1
        assert output_bus.wire_values == (
            0, 0, 0, 0, 0, 0, 0, 0, 1, 1, 1, 1, 1, 1, 1, 1)

        clock.value = 0
        data.value = 0
        clock.value = 1
        assert output_bus.wire_values == (
            0, 0, 0, 0, 0, 0, 0, 0, 0, 1, 1, 1, 1, 1, 1, 1)

        clock.value = 0
        data.value = 0
        clock.value = 1
        assert output_bus.wire_values == (
            0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 1, 1, 1, 1, 1, 1)

        clock.value = 0
        data.value = 0
        clock.value = 1
        assert output_bus.wire_values == (
            0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 1, 1, 1, 1, 1)

        clock.value = 0
        data.value = 0
        clock.value = 1
        assert output_bus.wire_values == (
            0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 1, 1, 1, 1)

        clock.value = 0
        data.value = 0
        clock.value = 1
        assert output_bus.wire_values == (
            0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 1, 1, 1)

        clock.value = 0
        data.value = 0
        clock.value = 1
        assert output_bus.wire_values == (
            0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 1, 1)

        clock.value = 0
        data.value = 0
        clock.value = 1
        assert output_bus.wire_values == (
            0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 1)

        clock.value = 0
        data.value = 0
        clock.value = 1
        assert output_bus.wire_values == (
            0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0)

        clock.value = 0
        data.value = 1
        clock.value = 1
        assert output_bus.wire_values == (
            1, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0)

        clock.value = 0
        data.value = 1
        clock.value = 1
        assert output_bus.wire_values == (
            1, 1, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0)

        clock.value = 0
        data.value = 1
        clock.value = 1
        assert output_bus.wire_values == (
            1, 1, 1, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0)

        clock.value = 0
        data.value = 1
        clock.value = 1
        assert output_bus.wire_values == (
            1, 1, 1, 1, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0)

        clock.value = 0
        data.value = 1
        clock.value = 1
        assert output_bus.wire_values == (
            1, 1, 1, 1, 1, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0)

        clock.value = 0
        data.value = 1
        clock.value = 1
        assert output_bus.wire_values == (
            1, 1, 1, 1, 1, 1, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0)

        clock.value = 0
        data.value = 1
        clock.value = 1
        assert output_bus.wire_values == (
            1, 1, 1, 1, 1, 1, 1, 0, 0, 0, 0, 0, 0, 0, 0, 0)

        clock.value = 0
        data.value = 1
        clock.value = 1
        assert output_bus.wire_values == (
            1, 1, 1, 1, 1, 1, 1, 1, 0, 0, 0, 0, 0, 0, 0, 0)

        clock.value = 0
        data.value = 1
        clock.value = 1
        assert output_bus.wire_values == (
            1, 1, 1, 1, 1, 1, 1, 1, 1, 0, 0, 0, 0, 0, 0, 0)

        clock.value = 0
        data.value = 1
        clock.value = 1
        assert output_bus.wire_values == (
            1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 0, 0, 0, 0, 0, 0)

        clock.value = 0
        data.value = 1
        clock.value = 1
        assert output_bus.wire_values == (
            1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 0, 0, 0, 0, 0)

        clock.value = 0
        data.value = 1
        clock.value = 1
        assert output_bus.wire_values == (
            1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 0, 0, 0, 0)

        clock.value = 0
        data.value = 1
        clock.value = 1
        assert output_bus.wire_values == (
            1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 0, 0, 0)

        clock.value = 0
        data.value = 1
        clock.value = 1
        assert output_bus.wire_values == (
            1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 0, 0)

        clock.value = 0
        data.value = 1
        clock.value = 1
        assert output_bus.wire_values == (
            1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 0)

        clock.value = 0
        data.value = 1
        clock.value = 1
        assert output_bus.wire_values == (
            1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1)

        enable.value = 0

        clock.value = 0
        data.value = 0
        clock.value = 1
        assert output_bus.wire_values == (
            1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1)

        enable.value = 1
        assert output_bus.wire_values == (
            1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1)

        clock.value = 0
        clock.value = 1
        assert output_bus.wire_values == (
            0, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1)

        clock.value = 0
        data.value = 0
        clock.value = 1
        assert output_bus.wire_values == (
            0, 0, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1)

        reset_n.value = 0
        assert output_bus.wire_values == (
            0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0)

        print(a.__doc__)
        print(a)
