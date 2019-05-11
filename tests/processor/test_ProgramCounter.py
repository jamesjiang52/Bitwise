import bitwise as bw


class TestProgramCounter:
    def test_ProgramCounter(self):
        input_bus = bw.wire.Bus16()
        up = bw.wire.Wire()
        load = bw.wire.Wire()
        clock = bw.wire.Wire()
        output_bus = bw.wire.Bus16()

        a = bw.processor.ProgramCounter(input_bus, up, load, clock, output_bus)

        clock.value = 0
        clock.value = 1
        assert output_bus.wire_values == (
            0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0)

        up.value = 1
        clock.value = 0
        clock.value = 1
        assert output_bus.wire_values == (
            0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 1)

        up.value = 1
        clock.value = 0
        clock.value = 1
        assert output_bus.wire_values == (
            0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 1, 0)

        up.value = 1
        clock.value = 0
        clock.value = 1
        assert output_bus.wire_values == (
            0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 1, 1)

        up.value = 0
        clock.value = 0
        clock.value = 1
        assert output_bus.wire_values == (
            0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 1, 1)

        clock.value = 0
        clock.value = 1
        assert output_bus.wire_values == (
            0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 1, 1)

        input_bus.wire_values = (
            0, 0, 0, 0, 0, 0, 0, 0, 1, 1, 1, 1, 1, 1, 1, 1)
        load.value = 1
        clock.value = 0
        clock.value = 1
        assert output_bus.wire_values == (
            0, 0, 0, 0, 0, 0, 0, 0, 1, 1, 1, 1, 1, 1, 1, 1)

        load.value = 0
        up.value = 1
        clock.value = 0
        clock.value = 1
        assert output_bus.wire_values == (
            0, 0, 0, 0, 0, 0, 0, 1, 0, 0, 0, 0, 0, 0, 0, 0)

        clock.value = 0
        clock.value = 1
        assert output_bus.wire_values == (
            0, 0, 0, 0, 0, 0, 0, 1, 0, 0, 0, 0, 0, 0, 0, 1)

        input_bus.wire_values = (
            0, 0, 0, 0, 0, 1, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0)
        load.value = 1
        clock.value = 0
        clock.value = 1
        assert output_bus.wire_values == (
            0, 0, 0, 0, 0, 1, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0)

        print(a.__doc__)
        print(a)

        a(
            data_bus=(0, 0, 0, 0, 0, 0, 0, 0, 1, 1, 1, 1, 1, 1, 1, 1),
            up=0,
            load=1,
            clock=0,
            output_bus=None
        )
        a(clock=1)
        assert output_bus.wire_values == (
            0, 0, 0, 0, 0, 0, 0, 0, 1, 1, 1, 1, 1, 1, 1, 1)
