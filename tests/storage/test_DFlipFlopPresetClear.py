import bitwise as bw


class TestDFlipFlopPresetClear:
    def test_DFlipFlopPresetClear(self):
        clock = bw.wire.Wire()
        preset_n = bw.wire.Wire()
        clear_n = bw.wire.Wire()
        data = bw.wire.Wire()
        output = bw.wire.Wire()
        output_not = bw.wire.Wire()

        a = bw.storage.DFlipFlopPresetClear(
            data,
            preset_n,
            clear_n,
            clock,
            output,
            output_not
        )

        preset_n.value = 1
        clear_n.value = 1

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

        preset_n.value = 0
        preset_n.value = 1
        assert output.value == 1
        assert output_not.value == 0

        clear_n.value = 0
        clear_n.value = 1
        assert output.value == 0
        assert output_not.value == 1

        print(a.__doc__)
        print(a)

        a(
            data=1,
            preset_n=1,
            clear_n=0,
            clock=0,
            output=None,
            output_not=None
        )
        assert output.value == 0
        assert output_not.value == 1
