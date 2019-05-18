import bitwise as bw


class TestDemultiplexer1To8:
    def test_Demultiplexer1To8(self):
        enable = bw.wire.Wire()
        select_1 = bw.wire.Wire()
        select_2 = bw.wire.Wire()
        select_3 = bw.wire.Wire()
        input_ = bw.wire.Wire()
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

        a = bw.signal.Demultiplexer1To8(
            enable,
            select_1,
            select_2,
            select_3,
            input_,
            output_bus
        )

        enable.value = 0
        select_1.value = 0
        select_2.value = 0
        select_3.value = 0
        input_.value = 0
        assert output_bus.wire_values == (0, 0, 0, 0, 0, 0, 0, 0)

        enable.value = 0
        select_1.value = 1
        select_2.value = 1
        select_3.value = 1
        input_.value = 1
        assert output_bus.wire_values == (0, 0, 0, 0, 0, 0, 0, 0)

        enable.value = 1
        select_1.value = 0
        select_2.value = 0
        select_3.value = 0
        input_.value = 0
        assert output_bus.wire_values == (0, 0, 0, 0, 0, 0, 0, 0)

        enable.value = 1
        select_1.value = 0
        select_2.value = 0
        select_3.value = 0
        input_.value = 1
        assert output_bus.wire_values == (0, 0, 0, 0, 0, 0, 0, 1)

        enable.value = 1
        select_1.value = 0
        select_2.value = 0
        select_3.value = 1
        input_.value = 0
        assert output_bus.wire_values == (0, 0, 0, 0, 0, 0, 0, 0)

        enable.value = 1
        select_1.value = 0
        select_2.value = 0
        select_3.value = 1
        input_.value = 1
        assert output_bus.wire_values == (0, 0, 0, 0, 0, 0, 1, 0)

        enable.value = 1
        select_1.value = 0
        select_2.value = 1
        select_3.value = 0
        input_.value = 0
        assert output_bus.wire_values == (0, 0, 0, 0, 0, 0, 0, 0)

        enable.value = 1
        select_1.value = 0
        select_2.value = 1
        select_3.value = 0
        input_.value = 1
        assert output_bus.wire_values == (0, 0, 0, 0, 0, 1, 0, 0)

        enable.value = 1
        select_1.value = 0
        select_2.value = 1
        select_3.value = 1
        input_.value = 0
        assert output_bus.wire_values == (0, 0, 0, 0, 0, 0, 0, 0)

        enable.value = 1
        select_1.value = 0
        select_2.value = 1
        select_3.value = 1
        input_.value = 1
        assert output_bus.wire_values == (0, 0, 0, 0, 1, 0, 0, 0)

        enable.value = 1
        select_1.value = 1
        select_2.value = 0
        select_3.value = 0
        input_.value = 0
        assert output_bus.wire_values == (0, 0, 0, 0, 0, 0, 0, 0)

        enable.value = 1
        select_1.value = 1
        select_2.value = 0
        select_3.value = 0
        input_.value = 1
        assert output_bus.wire_values == (0, 0, 0, 1, 0, 0, 0, 0)

        enable.value = 1
        select_1.value = 1
        select_2.value = 0
        select_3.value = 1
        input_.value = 0
        assert output_bus.wire_values == (0, 0, 0, 0, 0, 0, 0, 0)

        enable.value = 1
        select_1.value = 1
        select_2.value = 0
        select_3.value = 1
        input_.value = 1
        assert output_bus.wire_values == (0, 0, 1, 0, 0, 0, 0, 0)

        enable.value = 1
        select_1.value = 1
        select_2.value = 1
        select_3.value = 0
        input_.value = 0
        assert output_bus.wire_values == (0, 0, 0, 0, 0, 0, 0, 0)

        enable.value = 1
        select_1.value = 1
        select_2.value = 1
        select_3.value = 0
        input_.value = 1
        assert output_bus.wire_values == (0, 1, 0, 0, 0, 0, 0, 0)

        enable.value = 1
        select_1.value = 1
        select_2.value = 1
        select_3.value = 1
        input_.value = 0
        assert output_bus.wire_values == (0, 0, 0, 0, 0, 0, 0, 0)

        enable.value = 1
        select_1.value = 1
        select_2.value = 1
        select_3.value = 1
        input_.value = 1
        assert output_bus.wire_values == (1, 0, 0, 0, 0, 0, 0, 0)

        print(a.__doc__)
        print(a)

        a(
            enable=1,
            select_1=0,
            select_2=0,
            select_3=0,
            input=1,
            output_bus=None
        )
        assert output_bus.wire_values == (0, 0, 0, 0, 0, 0, 0, 1)
