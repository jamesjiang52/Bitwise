import bitwise as bw


class TestJKFlipFlopPresetClear:
    def test_JKFlipFlopPresetClear(self):
        clock = bw.wire.Wire()
        preset_n = bw.wire.Wire()
        clear_n = bw.wire.Wire()
        j = bw.wire.Wire()
        k = bw.wire.Wire()
        output = bw.wire.Wire()
        output_not = bw.wire.Wire()

        a = bw.storage.JKFlipFlopPresetClear(
            j,
            k,
            preset_n,
            clear_n,
            clock,
            output,
            output_not
        )

        preset_n.value = 1
        clear_n.value = 1

        clock.value = 0
        j.value = 1
        k.value = 0
        clock.value = 1
        assert output.value == 1
        assert output_not.value == 0

        clock.value = 0
        j.value = 0
        k.value = 1
        clock.value = 1
        assert output.value == 0
        assert output_not.value == 1

        clock.value = 0
        j.value = 0
        k.value = 0
        clock.value = 1
        assert output.value == 0
        assert output_not.value == 1

        clock.value = 0
        j.value = 1
        k.value = 1
        clock.value = 1
        assert output.value == 1
        assert output_not.value == 0

        clock.value = 0
        j.value = 0
        k.value = 0
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
