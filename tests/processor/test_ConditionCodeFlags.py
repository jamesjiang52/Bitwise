import bitwise as bw


class TestConditionCodeFlags:
    def test_ConditionCodeFlags(self):
        data_bus = bw.wire.Bus16()
        overflow = bw.wire.Wire()
        carry_out = bw.wire.Wire()
        enable = bw.wire.Wire()
        clock = bw.wire.Wire()
        z = bw.wire.Wire()
        v = bw.wire.Wire()
        n = bw.wire.Wire()
        c = bw.wire.Wire()
        flags = bw.wire.Bus4(z, v, n, c)

        a = bw.processor.ConditionCodeFlags(
            data_bus,
            overflow,
            carry_out,
            enable,
            clock,
            z, v, n, c
        )

        enable.value = 1

        data_bus.wire_values = (0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 1)
        overflow.value = 0
        carry_out.value = 0
        clock.value = 0
        clock.value = 1
        assert flags.wire_values == (0, 0, 0, 0)

        data_bus.wire_values = (0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0)
        overflow.value = 0
        carry_out.value = 0
        clock.value = 0
        clock.value = 1
        assert flags.wire_values == (1, 0, 0, 0)

        data_bus.wire_values = (0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0)
        overflow.value = 1
        carry_out.value = 0
        clock.value = 0
        clock.value = 1
        assert flags.wire_values == (1, 1, 0, 0)

        data_bus.wire_values = (1, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0)
        overflow.value = 0
        carry_out.value = 0
        clock.value = 0
        clock.value = 1
        assert flags.wire_values == (0, 0, 1, 0)

        data_bus.wire_values = (0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 1)
        overflow.value = 0
        carry_out.value = 1
        clock.value = 0
        clock.value = 1
        assert flags.wire_values == (0, 0, 0, 1)

        data_bus.wire_values = (1, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0)
        overflow.value = 0
        carry_out.value = 1
        clock.value = 0
        clock.value = 1
        assert flags.wire_values == (0, 0, 1, 1)

        data_bus.wire_values = (0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0)
        overflow.value = 1
        carry_out.value = 1
        clock.value = 0
        clock.value = 1
        assert flags.wire_values == (1, 1, 0, 1)

        enable.value = 0

        data_bus.wire_values = (1, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0)
        overflow.value = 0
        carry_out.value = 0
        clock.value = 0
        clock.value = 1
        assert flags.wire_values == (1, 1, 0, 1)

        data_bus.wire_values = (0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0)
        overflow.value = 0
        carry_out.value = 0
        clock.value = 0
        clock.value = 1
        assert flags.wire_values == (1, 1, 0, 1)

        print(a.__doc__)
        print(a)

        a(
            data_bus=(1, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0),
            overflow=0,
            carry_out=1,
            enable=1,
            clock=0,
            z=None,
            v=None,
            n=None,
            c=None
        )
        a(clock=1)
        assert flags.wire_values == (0, 0, 1, 1)
