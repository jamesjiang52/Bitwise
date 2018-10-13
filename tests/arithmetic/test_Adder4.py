import bitwise as bw


class TestAdder4:
    def test_Adder4(self):
        carry_in = bw.wire.Wire()
        input_1 = bw.wire.Wire()
        input_2 = bw.wire.Wire()
        input_3 = bw.wire.Wire()
        input_4 = bw.wire.Wire()
        input_5 = bw.wire.Wire()
        input_6 = bw.wire.Wire()
        input_7 = bw.wire.Wire()
        input_8 = bw.wire.Wire()
        carry_out = bw.wire.Wire()
        output_1 = bw.wire.Wire()
        output_2 = bw.wire.Wire()
        output_3 = bw.wire.Wire()
        output_4 = bw.wire.Wire()
        input_bus_1 = bw.wire.Bus4(input_1, input_2, input_3, input_4)
        input_bus_2 = bw.wire.Bus4(input_5, input_6, input_7, input_8)
        output_bus = bw.wire.Bus4(output_1, output_2, output_3, output_4)

        bw.arithmetic.Adder4(
            carry_in,
            input_bus_1,
            input_bus_2,
            carry_out,
            output_bus
        )

        carry_in.value = 0
        input_1.value = 0
        input_2.value = 0
        input_3.value = 0
        input_4.value = 0
        input_5.value = 0
        input_6.value = 0
        input_7.value = 0
        input_8.value = 0
        assert (carry_out.value, *output_bus.wire_values) == (0, 0, 0, 0, 0)

        carry_in.value = 0
        input_1.value = 1
        input_2.value = 1
        input_3.value = 1
        input_4.value = 1
        input_5.value = 0
        input_6.value = 0
        input_7.value = 0
        input_8.value = 0
        assert (carry_out.value, *output_bus.wire_values) == (0, 1, 1, 1, 1)

        carry_in.value = 0
        input_1.value = 1
        input_2.value = 1
        input_3.value = 1
        input_4.value = 1
        input_5.value = 0
        input_6.value = 0
        input_7.value = 0
        input_8.value = 1
        assert (carry_out.value, *output_bus.wire_values) == (1, 0, 0, 0, 0)

        carry_in.value = 0
        input_1.value = 1
        input_2.value = 1
        input_3.value = 1
        input_4.value = 1
        input_5.value = 0
        input_6.value = 0
        input_7.value = 1
        input_8.value = 0
        assert (carry_out.value, *output_bus.wire_values) == (1, 0, 0, 0, 1)

        carry_in.value = 0
        input_1.value = 1
        input_2.value = 1
        input_3.value = 1
        input_4.value = 1
        input_5.value = 0
        input_6.value = 1
        input_7.value = 0
        input_8.value = 0
        assert (carry_out.value, *output_bus.wire_values) == (1, 0, 0, 1, 1)

        carry_in.value = 0
        input_1.value = 1
        input_2.value = 1
        input_3.value = 1
        input_4.value = 1
        input_5.value = 1
        input_6.value = 0
        input_7.value = 0
        input_8.value = 0
        assert (carry_out.value, *output_bus.wire_values) == (1, 0, 1, 1, 1)

        carry_in.value = 0
        input_1.value = 0
        input_2.value = 0
        input_3.value = 0
        input_4.value = 0
        input_5.value = 1
        input_6.value = 1
        input_7.value = 1
        input_8.value = 1
        assert (carry_out.value, *output_bus.wire_values) == (0, 1, 1, 1, 1)

        carry_in.value = 0
        input_1.value = 0
        input_2.value = 0
        input_3.value = 0
        input_4.value = 1
        input_5.value = 1
        input_6.value = 1
        input_7.value = 1
        input_8.value = 1
        assert (carry_out.value, *output_bus.wire_values) == (1, 0, 0, 0, 0)

        carry_in.value = 0
        input_1.value = 0
        input_2.value = 0
        input_3.value = 1
        input_4.value = 0
        input_5.value = 1
        input_6.value = 1
        input_7.value = 1
        input_8.value = 1
        assert (carry_out.value, *output_bus.wire_values) == (1, 0, 0, 0, 1)

        carry_in.value = 0
        input_1.value = 0
        input_2.value = 1
        input_3.value = 0
        input_4.value = 0
        input_5.value = 1
        input_6.value = 1
        input_7.value = 1
        input_8.value = 1
        assert (carry_out.value, *output_bus.wire_values) == (1, 0, 0, 1, 1)

        carry_in.value = 0
        input_1.value = 1
        input_2.value = 0
        input_3.value = 0
        input_4.value = 0
        input_5.value = 1
        input_6.value = 1
        input_7.value = 1
        input_8.value = 1
        assert (carry_out.value, *output_bus.wire_values) == (1, 0, 1, 1, 1)

        carry_in.value = 0
        input_1.value = 1
        input_2.value = 1
        input_3.value = 1
        input_4.value = 1
        input_5.value = 1
        input_6.value = 1
        input_7.value = 1
        input_8.value = 1
        assert (carry_out.value, *output_bus.wire_values) == (1, 1, 1, 1, 0)

        carry_in.value = 1
        input_1.value = 0
        input_2.value = 0
        input_3.value = 0
        input_4.value = 0
        input_5.value = 0
        input_6.value = 0
        input_7.value = 0
        input_8.value = 0
        assert (carry_out.value, *output_bus.wire_values) == (0, 0, 0, 0, 1)

        carry_in.value = 1
        input_1.value = 1
        input_2.value = 1
        input_3.value = 1
        input_4.value = 1
        input_5.value = 0
        input_6.value = 0
        input_7.value = 0
        input_8.value = 0
        assert (carry_out.value, *output_bus.wire_values) == (1, 0, 0, 0, 0)

        carry_in.value = 1
        input_1.value = 1
        input_2.value = 1
        input_3.value = 1
        input_4.value = 1
        input_5.value = 0
        input_6.value = 0
        input_7.value = 0
        input_8.value = 1
        assert (carry_out.value, *output_bus.wire_values) == (1, 0, 0, 0, 1)

        carry_in.value = 1
        input_1.value = 1
        input_2.value = 1
        input_3.value = 1
        input_4.value = 1
        input_5.value = 0
        input_6.value = 0
        input_7.value = 1
        input_8.value = 0
        assert (carry_out.value, *output_bus.wire_values) == (1, 0, 0, 1, 0)

        carry_in.value = 1
        input_1.value = 1
        input_2.value = 1
        input_3.value = 1
        input_4.value = 1
        input_5.value = 0
        input_6.value = 1
        input_7.value = 0
        input_8.value = 0
        assert (carry_out.value, *output_bus.wire_values) == (1, 0, 1, 0, 0)

        carry_in.value = 1
        input_1.value = 1
        input_2.value = 1
        input_3.value = 1
        input_4.value = 1
        input_5.value = 1
        input_6.value = 0
        input_7.value = 0
        input_8.value = 0
        assert (carry_out.value, *output_bus.wire_values) == (1, 1, 0, 0, 0)

        carry_in.value = 1
        input_1.value = 0
        input_2.value = 0
        input_3.value = 0
        input_4.value = 0
        input_5.value = 1
        input_6.value = 1
        input_7.value = 1
        input_8.value = 1
        assert (carry_out.value, *output_bus.wire_values) == (1, 0, 0, 0, 0)

        carry_in.value = 1
        input_1.value = 0
        input_2.value = 0
        input_3.value = 0
        input_4.value = 1
        input_5.value = 1
        input_6.value = 1
        input_7.value = 1
        input_8.value = 1
        assert (carry_out.value, *output_bus.wire_values) == (1, 0, 0, 0, 1)

        carry_in.value = 1
        input_1.value = 0
        input_2.value = 0
        input_3.value = 1
        input_4.value = 0
        input_5.value = 1
        input_6.value = 1
        input_7.value = 1
        input_8.value = 1
        assert (carry_out.value, *output_bus.wire_values) == (1, 0, 0, 1, 0)

        carry_in.value = 1
        input_1.value = 0
        input_2.value = 1
        input_3.value = 0
        input_4.value = 0
        input_5.value = 1
        input_6.value = 1
        input_7.value = 1
        input_8.value = 1
        assert (carry_out.value, *output_bus.wire_values) == (1, 0, 1, 0, 0)

        carry_in.value = 1
        input_1.value = 1
        input_2.value = 0
        input_3.value = 0
        input_4.value = 0
        input_5.value = 1
        input_6.value = 1
        input_7.value = 1
        input_8.value = 1
        assert (carry_out.value, *output_bus.wire_values) == (1, 1, 0, 0, 0)

        carry_in.value = 1
        input_1.value = 1
        input_2.value = 1
        input_3.value = 1
        input_4.value = 1
        input_5.value = 1
        input_6.value = 1
        input_7.value = 1
        input_8.value = 1
        assert (carry_out.value, *output_bus.wire_values) == (1, 1, 1, 1, 1)
