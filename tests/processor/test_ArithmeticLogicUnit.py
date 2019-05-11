import bitwise as bw


class TestArithmeticLogicUnit:
    def test_ArithmeticLogicUnit(self):
        a_bus = bw.wire.Bus16()
        b_bus = bw.wire.Bus16()

        function_select_bus = bw.wire.Bus4()
        overflow = bw.wire.Wire()
        carry_out = bw.wire.Wire()
        output_bus = bw.wire.Bus16()

        a = bw.processor.ArithmeticLogicUnit(
            a_bus,
            b_bus,
            function_select_bus,
            overflow,
            carry_out,
            output_bus
        )

        a_bus.wire_values = (0, 0, 0, 0, 1, 1, 1, 1, 0, 0, 0, 0, 1, 1, 1, 1)
        b_bus.wire_values = (0, 1, 0, 1, 0, 1, 0, 1, 0, 1, 0, 1, 0, 1, 0, 1)

        function_select_bus.wire_values = (0, 0, 0, 0)
        assert output_bus.wire_values == (
            0, 0, 0, 0, 1, 1, 1, 1, 0, 0, 0, 0, 1, 1, 1, 1)

        function_select_bus.wire_values = (0, 0, 0, 1)
        assert output_bus.wire_values == (
            1, 1, 1, 1, 0, 0, 0, 0, 1, 1, 1, 1, 0, 0, 0, 0)

        function_select_bus.wire_values = (0, 0, 1, 0)
        assert output_bus.wire_values == (
            0, 1, 0, 1, 0, 1, 0, 1, 0, 1, 0, 1, 0, 1, 0, 1)

        function_select_bus.wire_values = (0, 0, 1, 1)
        assert output_bus.wire_values == (
            1, 0, 1, 0, 1, 0, 1, 0, 1, 0, 1, 0, 1, 0, 1, 0)

        function_select_bus.wire_values = (0, 1, 0, 0)
        assert output_bus.wire_values == (
            0, 0, 0, 0, 0, 1, 0, 1, 0, 0, 0, 0, 0, 1, 0, 1)

        function_select_bus.wire_values = (0, 1, 0, 1)
        assert output_bus.wire_values == (
            1, 1, 1, 1, 1, 0, 1, 0, 1, 1, 1, 1, 1, 0, 1, 0)

        function_select_bus.wire_values = (0, 1, 1, 0)
        assert output_bus.wire_values == (
            0, 1, 0, 1, 1, 1, 1, 1, 0, 1, 0, 1, 1, 1, 1, 1)

        function_select_bus.wire_values = (0, 1, 1, 1)
        assert output_bus.wire_values == (
            1, 0, 1, 0, 0, 0, 0, 0, 1, 0, 1, 0, 0, 0, 0, 0)

        function_select_bus.wire_values = (1, 0, 0, 0)
        assert output_bus.wire_values == (
            0, 1, 0, 1, 1, 0, 1, 0, 0, 1, 0, 1, 1, 0, 1, 0)

        function_select_bus.wire_values = (1, 0, 0, 1)
        assert output_bus.wire_values == (
            1, 0, 1, 0, 0, 1, 0, 1, 1, 0, 1, 0, 0, 1, 0, 1)

        function_select_bus.wire_values = (1, 0, 1, 0)
        assert output_bus.wire_values == (
            0, 1, 1, 0, 0, 1, 0, 0, 0, 1, 1, 0, 0, 1, 0, 0)
        assert carry_out.value == 0

        function_select_bus.wire_values = (1, 0, 1, 1)
        assert output_bus.wire_values == (
            1, 0, 0, 1, 1, 0, 1, 1, 1, 0, 0, 1, 1, 0, 1, 1)

        function_select_bus.wire_values = (1, 1, 0, 0)
        assert output_bus.wire_values == (
            1, 0, 1, 1, 1, 0, 0, 1, 1, 0, 1, 1, 1, 0, 1, 0)
        assert overflow.value == 0

        function_select_bus.wire_values = (1, 1, 0, 1)
        assert output_bus.wire_values == (
            0, 1, 0, 0, 0, 1, 1, 0, 0, 1, 0, 0, 0, 1, 0, 1)

        function_select_bus.wire_values = (1, 1, 1, 0)
        assert output_bus.wire_values == (
            0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0)

        function_select_bus.wire_values = (1, 1, 1, 1)
        assert output_bus.wire_values == (
            1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1)

        print(a.__doc__)
        print(a)

        a(
            a_bus=(0, 0, 0, 0, 1, 1, 1, 1, 0, 0, 0, 0, 1, 1, 1, 1),
            b_bus=(0, 1, 0, 1, 0, 1, 0, 1, 0, 1, 0, 1, 0, 1, 0, 1),
            function_select_bus=(1, 0, 1, 0),
            overflow=None,
            carry_out=None,
            output_bus=None
        )
        assert output_bus.wire_values == (
            0, 1, 1, 0, 0, 1, 0, 0, 0, 1, 1, 0, 0, 1, 0, 0)
        assert carry_out.value == 0
