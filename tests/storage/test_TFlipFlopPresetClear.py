import bitwise as bw


class TestTFlipFlopPresetClear:
    def test_TFlipFlopPresetClear(self):
        clock = bw.wire.Wire()
        preset_n = bw.wire.Wire()
        clear_n = bw.wire.Wire()
        toggle = bw.wire.Wire()
        output = bw.wire.Wire()
        output_not = bw.wire.Wire()

        a = bw.storage.TFlipFlopPresetClear(
            toggle,
            preset_n,
            clear_n,
            clock,
            output,
            output_not
        )

        preset_n.value = 1
        clear_n.value = 1

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

        clear_n.value = 0
        clear_n.value = 1
        assert output.value == 0
        assert output_not.value == 1

        preset_n.value = 0
        preset_n.value = 1
        assert output.value == 1
        assert output_not.value == 0

        print(a.__doc__)
        print(a)
