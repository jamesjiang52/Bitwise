import bitwise as bw


class TestStackPointer:
    def test_StackPointer(self):
        up = bw.wire.Wire()
        down = bw.wire.Wire()
        clock = bw.wire.Wire()
        output_bus = bw.wire.Bus16()

        a = bw.processor.StackPointer(up, down, clock, output_bus)

        clock.value = 0
        clock.value = 1
        assert output_bus.wire_values == (
            0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0)

        down.value = 1
        clock.value = 0
        clock.value = 1
        assert output_bus.wire_values == (
            1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1)

        clock.value = 0
        clock.value = 1
        assert output_bus.wire_values == (
            1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 0)

        clock.value = 0
        clock.value = 1
        assert output_bus.wire_values == (
            1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 0, 1)

        clock.value = 0
        clock.value = 1
        assert output_bus.wire_values == (
            1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 0, 0)

        down.value = 0
        clock.value = 0
        clock.value = 1
        assert output_bus.wire_values == (
            1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 0, 0)

        clock.value = 0
        clock.value = 1
        assert output_bus.wire_values == (
            1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 0, 0)

        clock.value = 0
        clock.value = 1
        assert output_bus.wire_values == (
            1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 0, 0)

        up.value = 1
        clock.value = 0
        clock.value = 1
        assert output_bus.wire_values == (
            1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 0, 1)

        clock.value = 0
        clock.value = 1
        assert output_bus.wire_values == (
            1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 0)

        down.value = 1
        clock.value = 0
        clock.value = 1
        assert output_bus.wire_values == (
            1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 0, 1)

        print(a.__doc__)
        print(a)

        a(
            up=0,
            down=1,
            clock=0,
            output_bus=None
        )
        a(clock=1)
        assert output_bus.wire_values == (
            1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 0, 0)
