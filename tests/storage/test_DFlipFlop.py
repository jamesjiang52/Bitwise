import bitwise as bw


class TestDFlipFlop:
    def test_DFlipFlop(self):
        clock = bw.wire.Wire()
        data = bw.wire.Wire()
        output = bw.wire.Wire()
        output_not = bw.wire.Wire()

        a = bw.storage.DFlipFlop(data, clock, output, output_not)

        clock.value = 0
        data.value = 1
        clock.value = 1
        assert output.value == 1
        assert output_not.value == 0

        clock.value = 1
        data.value = 1
        assert output.value == 1
        assert output_not.value == 0

        clock.value = 0
        data.value = 0
        assert output.value == 1
        assert output_not.value == 0

        clock.value = 0
        data.value = 0
        assert output.value == 1
        assert output_not.value == 0

        clock.value = 0
        data.value = 1
        assert output.value == 1
        assert output_not.value == 0

        clock.value = 0
        data.value = 0
        clock.value = 1
        assert output.value == 0
        assert output_not.value == 1

        clock.value = 1
        data.value = 0
        assert output.value == 0
        assert output_not.value == 1

        clock.value = 1
        data.value = 1
        assert output.value == 0
        assert output_not.value == 1

        clock.value = 0
        data.value = 1
        assert output.value == 0
        assert output_not.value == 1

        clock.value = 0
        data.value = 0
        assert output.value == 0
        assert output_not.value == 1

        print(a.__doc__)
        print(a)

        a(data=1, clock=0, output=None, output_not=None)
        a(clock=1)
        assert output.value == 1
        assert output_not.value == 0
