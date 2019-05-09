import bitwise as bw


class TestTFlipFlop:
    def test_TFlipFlop(self):
        clock = bw.wire.Wire()
        toggle = bw.wire.Wire()
        output = bw.wire.Wire()
        output_not = bw.wire.Wire()

        a = bw.storage.TFlipFlop(toggle, clock, output, output_not)

        clock.value = 0
        toggle.value = 1
        clock.value = 1
        assert output.value == 1
        assert output_not.value == 0

        clock.value = 0
        toggle.value = 1
        clock.value = 1
        assert output.value == 0
        assert output_not.value == 1

        clock.value = 0
        toggle.value = 0
        clock.value = 1
        assert output.value == 0
        assert output_not.value == 1

        clock.value = 0
        toggle.value = 1
        clock.value = 1
        assert output.value == 1
        assert output_not.value == 0

        print(a.__doc__)
        print(a)

        a(toggle=1, clock=0, output=None, output_not=None)
        a(clock=1)
        assert output.value == 0
        assert output_not.value == 1
