import bitwise as bw


class TestArithmeticLogicUnit:
    def test_ArithmeticLogicUnit(self):
        a_1 = bw.wire.Wire()
        a_2 = bw.wire.Wire()
        a_3 = bw.wire.Wire()
        a_4 = bw.wire.Wire()
        a_5 = bw.wire.Wire()
        a_6 = bw.wire.Wire()
        a_7 = bw.wire.Wire()
        a_8 = bw.wire.Wire()

        b_1 = bw.wire.Wire()
        b_2 = bw.wire.Wire()
        b_3 = bw.wire.Wire()
        b_4 = bw.wire.Wire()
        b_5 = bw.wire.Wire()
        b_6 = bw.wire.Wire()
        b_7 = bw.wire.Wire()
        b_8 = bw.wire.Wire()

        a_bus = bw.wire.Bus8(a_1, a_2, a_3, a_4, a_5, a_6, a_7, a_8)
        b_bus = bw.wire.Bus8(b_1, b_2, b_3, b_4, b_5, b_6, b_7, b_8)

        function_select_bus = bw.wire.Bus4()
        overflow = bw.wire.Wire()
        carry_out = bw.wire.Wire()
        output_bus = bw.wire.Bus8()
        greater_than = bw.wire.Wire()
        equal_to = bw.wire.Wire()
        less_than = bw.wire.Wire()

        bw.processor.ArithmeticLogicUnit(
            a_bus,
            b_bus,
            function_select_bus,
            overflow,
            carry_out,
            output_bus,
            greater_than,
            equal_to,
            less_than
        )

        a_bus.wire_values = (0, 0, 0, 0, 1, 1, 1, 1)
        b_bus.wire_values = (0, 1, 0, 1, 0, 1, 0, 1)

        assert greater_than.value == 0
        assert equal_to.value == 0
        assert less_than.value == 1

        function_select_bus.wire_values = (0, 0, 0, 0)
        assert output_bus.wire_values == (0, 0, 0, 0, 1, 1, 1, 1)

        function_select_bus.wire_values = (0, 0, 0, 1)
        assert output_bus.wire_values == (1, 1, 1, 1, 0, 0, 0, 0)

        function_select_bus.wire_values = (0, 0, 1, 0)
        assert output_bus.wire_values == (0, 1, 0, 1, 0, 1, 0, 1)

        function_select_bus.wire_values = (0, 0, 1, 1)
        assert output_bus.wire_values == (1, 0, 1, 0, 1, 0, 1, 0)

        function_select_bus.wire_values = (0, 1, 0, 0)
        assert output_bus.wire_values == (0, 0, 0, 0, 0, 1, 0, 1)

        function_select_bus.wire_values = (0, 1, 0, 1)
        assert output_bus.wire_values == (1, 1, 1, 1, 1, 0, 1, 0)

        function_select_bus.wire_values = (0, 1, 1, 0)
        assert output_bus.wire_values == (0, 1, 0, 1, 1, 1, 1, 1)

        function_select_bus.wire_values = (0, 1, 1, 1)
        assert output_bus.wire_values == (1, 0, 1, 0, 0, 0, 0, 0)

        function_select_bus.wire_values = (1, 0, 0, 0)
        assert output_bus.wire_values == (0, 1, 0, 1, 1, 0, 1, 0)

        function_select_bus.wire_values = (1, 0, 0, 1)
        assert output_bus.wire_values == (1, 0, 1, 0, 0, 1, 0, 1)

        function_select_bus.wire_values = (1, 0, 1, 0)
        assert output_bus.wire_values == (0, 1, 1, 0, 0, 1, 0, 0)
        assert carry_out.value == 0

        function_select_bus.wire_values = (1, 0, 1, 1)
        assert output_bus.wire_values == (1, 0, 0, 1, 1, 0, 1, 1)

        function_select_bus.wire_values = (1, 1, 0, 0)
        assert output_bus.wire_values == (1, 0, 1, 1, 1, 0, 1, 0)
        assert overflow.value == 0

        function_select_bus.wire_values = (1, 1, 0, 1)
        assert output_bus.wire_values == (0, 1, 0, 0, 0, 1, 0, 1)

        function_select_bus.wire_values = (1, 1, 1, 0)
        assert output_bus.wire_values == (1, 1, 1, 1, 1, 0, 1, 1)

        function_select_bus.wire_values = (1, 1, 1, 1)
        assert output_bus.wire_values == (0, 0, 0, 0, 0, 1, 0, 0)
