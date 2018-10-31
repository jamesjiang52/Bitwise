import bitwise as bw


class TestDownCounterMod4:
    def test_DownCounterMod4(self):
        enable = bw.wire.Wire()
        load_n = bw.wire.Wire()
        clock = bw.wire.Wire()
        load_1 = bw.wire.Wire()
        load_2 = bw.wire.Wire()
        output_1 = bw.wire.Wire()
        output_2 = bw.wire.Wire()

        bw.storage.DownCounterMod4(
            enable,
            load_n,
            load_1,
            load_2,
            clock,
            output_1,
            output_2
        )

        enable.value = 1

        load_n.value = 0
        load_1.value = 1
        load_2.value = 1
        clock.value = 0
        clock.value = 1
        load_n.value = 1
        assert (output_1.value, output_2.value) == (1, 1)

        clock.value = 0
        clock.value = 1
        assert (output_1.value, output_2.value) == (1, 0)

        clock.value = 0
        clock.value = 1
        assert (output_1.value, output_2.value) == (0, 1)

        clock.value = 0
        clock.value = 1
        assert (output_1.value, output_2.value) == (0, 0)

        clock.value = 0
        clock.value = 1
        assert (output_1.value, output_2.value) == (1, 1)

        clock.value = 0
        clock.value = 1
        assert (output_1.value, output_2.value) == (1, 0)

        enable.value = 0

        clock.value = 0
        clock.value = 1
        assert (output_1.value, output_2.value) == (1, 0)

        enable.value = 1

        clock.value = 0
        clock.value = 1
        assert (output_1.value, output_2.value) == (0, 1)

        load_n.value = 0
        load_1.value = 1
        load_2.value = 0
        clock.value = 0
        clock.value = 1
        load_n.value = 1
        assert (output_1.value, output_2.value) == (1, 0)
