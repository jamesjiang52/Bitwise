import bitwise as bw


class TestRegister4:
    def test_Register4(self):
        input_1 = bw.wire.Wire()
        input_2 = bw.wire.Wire()
        input_3 = bw.wire.Wire()
        input_4 = bw.wire.Wire()
        clock = bw.wire.Wire()
        output_1 = bw.wire.Wire()
        output_2 = bw.wire.Wire()
        output_3 = bw.wire.Wire()
        output_4 = bw.wire.Wire()
        input_bus = bw.wire.Bus4(input_1, input_2, input_3, input_4)
        output_bus = bw.wire.Bus4(output_1, output_2, output_3, output_4)

        bw.storage.Register4(input_bus, clock, output_bus)

        clock.value = 0
        input_1.value = 0
        input_2.value = 0
        input_3.value = 0
        input_4.value = 0
        clock.value = 1
        assert output_bus.wire_values == (0, 0, 0, 0)

        clock.value = 0
        input_4.value = 1
        clock.value = 1
        assert output_bus.wire_values == (0, 0, 0, 1)

        clock.value = 0
        input_3.value = 1
        clock.value = 1
        assert output_bus.wire_values == (0, 0, 1, 1)

        clock.value = 0
        input_2.value = 1
        clock.value = 1
        assert output_bus.wire_values == (0, 1, 1, 1)

        clock.value = 0
        input_1.value = 1
        clock.value = 1
        assert output_bus.wire_values == (1, 1, 1, 1)

        clock.value = 0
        input_1.value = 0
        input_2.value = 0
        input_3.value = 0
        input_4.value = 0
        clock.value = 1
        assert output_bus.wire_values == (0, 0, 0, 0)