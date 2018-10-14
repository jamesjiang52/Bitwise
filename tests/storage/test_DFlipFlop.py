import bitwise as bw


class TestDFlipFlop:
    def test_DFlipFlop(self):
        clock = bw.wire.Wire()
        data = bw.wire.Wire()
        output = bw.wire.Wire()
        output_not = bw.wire.Wire()

        bw.storage.DFlipFlop(data, clock, output, output_not)

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
