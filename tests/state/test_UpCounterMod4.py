import bitwise as bw


class TestUpCounterMod4:
    def test_UpCounterMod4(self):
        enable = bw.wire.Wire()
        clear_n = bw.wire.Wire()
        clock = bw.wire.Wire()
        output_1 = bw.wire.Wire()
        output_2 = bw.wire.Wire()

        bw.state.UpCounterMod4(enable, clear_n, clock, output_1, output_2)

        clear_n.value = 0
        clear_n.value = 1
        assert (output_1.value, output_2.value) == (0, 0)

        enable.value = 1

        clock.value = 0
        clock.value = 1
        assert (output_1.value, output_2.value) == (0, 1)

        clock.value = 0
        clock.value = 1
        assert (output_1.value, output_2.value) == (1, 0)

        clock.value = 0
        clock.value = 1
        assert (output_1.value, output_2.value) == (1, 1)

        clock.value = 0
        clock.value = 1
        assert (output_1.value, output_2.value) == (0, 0)

        clock.value = 0
        clock.value = 1
        assert (output_1.value, output_2.value) == (0, 1)

        enable.value = 0

        clock.value = 0
        clock.value = 1
        assert (output_1.value, output_2.value) == (0, 1)

        enable.value = 1

        clock.value = 0
        clock.value = 1
        assert (output_1.value, output_2.value) == (1, 0)

        clear_n.value = 0
        assert (output_1.value, output_2.value) == (0, 0)
