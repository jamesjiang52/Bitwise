import bitwise as bw


class TestDemultiplexer1To16:
    def test_Demultiplexer1To16(self):
        enable = bw.wire.Wire()
        select_1 = bw.wire.Wire()
        select_2 = bw.wire.Wire()
        select_3 = bw.wire.Wire()
        select_4 = bw.wire.Wire()
        input_ = bw.wire.Wire()
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
        select_bus = bw.wire.Bus4(select_1, select_2, select_3, select_4)
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

        a = bw.signal.Demultiplexer1To16(
            enable, select_bus, input_, output_bus
        )

        enable.value = 0
        select_1.value = 0
        select_2.value = 0
        select_3.value = 0
        select_4.value = 0
        input_.value = 0
        assert output_bus.wire_values == (
            0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0)

        enable.value = 0
        select_1.value = 1
        select_2.value = 1
        select_3.value = 1
        select_4.value = 1
        input_.value = 1
        assert output_bus.wire_values == (
            0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0)

        enable.value = 1
        select_1.value = 0
        select_2.value = 0
        select_3.value = 0
        select_4.value = 0
        input_.value = 0
        assert output_bus.wire_values == (
            0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0)

        enable.value = 1
        select_1.value = 0
        select_2.value = 0
        select_3.value = 0
        select_4.value = 0
        input_.value = 1
        assert output_bus.wire_values == (
            0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 1)

        enable.value = 1
        select_1.value = 0
        select_2.value = 0
        select_3.value = 0
        select_4.value = 1
        input_.value = 0
        assert output_bus.wire_values == (
            0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0)

        enable.value = 1
        select_1.value = 0
        select_2.value = 0
        select_3.value = 0
        select_4.value = 1
        input_.value = 1
        assert output_bus.wire_values == (
            0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 1, 0)

        enable.value = 1
        select_1.value = 0
        select_2.value = 0
        select_3.value = 1
        select_4.value = 0
        input_.value = 0
        assert output_bus.wire_values == (
            0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0)

        enable.value = 1
        select_1.value = 0
        select_2.value = 0
        select_3.value = 1
        select_4.value = 0
        input_.value = 1
        assert output_bus.wire_values == (
            0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 1, 0, 0)

        enable.value = 1
        select_1.value = 0
        select_2.value = 0
        select_3.value = 1
        select_4.value = 1
        input_.value = 0
        assert output_bus.wire_values == (
            0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0)

        enable.value = 1
        select_1.value = 0
        select_2.value = 0
        select_3.value = 1
        select_4.value = 1
        input_.value = 1
        assert output_bus.wire_values == (
            0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 1, 0, 0, 0)

        enable.value = 1
        select_1.value = 0
        select_2.value = 1
        select_3.value = 0
        select_4.value = 0
        input_.value = 0
        assert output_bus.wire_values == (
            0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0)

        enable.value = 1
        select_1.value = 0
        select_2.value = 1
        select_3.value = 0
        select_4.value = 0
        input_.value = 1
        assert output_bus.wire_values == (
            0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 1, 0, 0, 0, 0)

        enable.value = 1
        select_1.value = 0
        select_2.value = 1
        select_3.value = 0
        select_4.value = 1
        input_.value = 0
        assert output_bus.wire_values == (
            0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0)

        enable.value = 1
        select_1.value = 0
        select_2.value = 1
        select_3.value = 0
        select_4.value = 1
        input_.value = 1
        assert output_bus.wire_values == (
            0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 1, 0, 0, 0, 0, 0)

        enable.value = 1
        select_1.value = 0
        select_2.value = 1
        select_3.value = 1
        select_4.value = 0
        input_.value = 0
        assert output_bus.wire_values == (
            0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0)

        enable.value = 1
        select_1.value = 0
        select_2.value = 1
        select_3.value = 1
        select_4.value = 0
        input_.value = 1
        assert output_bus.wire_values == (
            0, 0, 0, 0, 0, 0, 0, 0, 0, 1, 0, 0, 0, 0, 0, 0)

        enable.value = 1
        select_1.value = 0
        select_2.value = 1
        select_3.value = 1
        select_4.value = 1
        input_.value = 0
        assert output_bus.wire_values == (
            0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0)

        enable.value = 1
        select_1.value = 0
        select_2.value = 1
        select_3.value = 1
        select_4.value = 1
        input_.value = 1
        assert output_bus.wire_values == (
            0, 0, 0, 0, 0, 0, 0, 0, 1, 0, 0, 0, 0, 0, 0, 0)

        enable.value = 1
        select_1.value = 1
        select_2.value = 0
        select_3.value = 0
        select_4.value = 0
        input_.value = 0
        assert output_bus.wire_values == (
            0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0)

        enable.value = 1
        select_1.value = 1
        select_2.value = 0
        select_3.value = 0
        select_4.value = 0
        input_.value = 1
        assert output_bus.wire_values == (
            0, 0, 0, 0, 0, 0, 0, 1, 0, 0, 0, 0, 0, 0, 0, 0)

        enable.value = 1
        select_1.value = 1
        select_2.value = 0
        select_3.value = 0
        select_4.value = 1
        input_.value = 0
        assert output_bus.wire_values == (
            0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0)

        enable.value = 1
        select_1.value = 1
        select_2.value = 0
        select_3.value = 0
        select_4.value = 1
        input_.value = 1
        assert output_bus.wire_values == (
            0, 0, 0, 0, 0, 0, 1, 0, 0, 0, 0, 0, 0, 0, 0, 0)

        enable.value = 1
        select_1.value = 1
        select_2.value = 0
        select_3.value = 1
        select_4.value = 0
        input_.value = 0
        assert output_bus.wire_values == (
            0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0)

        enable.value = 1
        select_1.value = 1
        select_2.value = 0
        select_3.value = 1
        select_4.value = 0
        input_.value = 1
        assert output_bus.wire_values == (
            0, 0, 0, 0, 0, 1, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0)

        enable.value = 1
        select_1.value = 1
        select_2.value = 0
        select_3.value = 1
        select_4.value = 1
        input_.value = 0
        assert output_bus.wire_values == (
            0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0)

        enable.value = 1
        select_1.value = 1
        select_2.value = 0
        select_3.value = 1
        select_4.value = 1
        input_.value = 1
        assert output_bus.wire_values == (
            0, 0, 0, 0, 1, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0)

        enable.value = 1
        select_1.value = 1
        select_2.value = 1
        select_3.value = 0
        select_4.value = 0
        input_.value = 0
        assert output_bus.wire_values == (
            0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0)

        enable.value = 1
        select_1.value = 1
        select_2.value = 1
        select_3.value = 0
        select_4.value = 0
        input_.value = 1
        assert output_bus.wire_values == (
            0, 0, 0, 1, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0)

        enable.value = 1
        select_1.value = 1
        select_2.value = 1
        select_3.value = 0
        select_4.value = 1
        input_.value = 0
        assert output_bus.wire_values == (
            0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0)

        enable.value = 1
        select_1.value = 1
        select_2.value = 1
        select_3.value = 0
        select_4.value = 1
        input_.value = 1
        assert output_bus.wire_values == (
            0, 0, 1, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0)

        enable.value = 1
        select_1.value = 1
        select_2.value = 1
        select_3.value = 1
        select_4.value = 0
        input_.value = 0
        assert output_bus.wire_values == (
            0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0)

        enable.value = 1
        select_1.value = 1
        select_2.value = 1
        select_3.value = 1
        select_4.value = 0
        input_.value = 1
        assert output_bus.wire_values == (
            0, 1, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0)

        enable.value = 1
        select_1.value = 1
        select_2.value = 1
        select_3.value = 1
        select_4.value = 1
        input_.value = 0
        assert output_bus.wire_values == (
            0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0)

        enable.value = 1
        select_1.value = 1
        select_2.value = 1
        select_3.value = 1
        select_4.value = 1
        input_.value = 1
        assert output_bus.wire_values == (
            1, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0)

        print(a.__doc__)
        print(a)

        a(
            enable=1,
            select_bus=(0, 0, 0, 0),
            input=1,
            output_bus=None
        )
        assert output_bus.wire_values == (
            0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 1)
