import bitwise as bw


class TestParallelToSerialConverter8To1:
    def test_ParallelToSerialConverter8To1(self):
        enable = bw.wire.Wire()
        reset_n = bw.wire.Wire()
        parallel_load_n = bw.wire.Wire()
        data_1 = bw.wire.Wire()
        data_2 = bw.wire.Wire()
        data_3 = bw.wire.Wire()
        data_4 = bw.wire.Wire()
        data_5 = bw.wire.Wire()
        data_6 = bw.wire.Wire()
        data_7 = bw.wire.Wire()
        data_8 = bw.wire.Wire()
        clock = bw.wire.Wire()
        output = bw.wire.Wire()
        data_bus = bw.wire.Bus8(
            data_1,
            data_2,
            data_3,
            data_4,
            data_5,
            data_6,
            data_7,
            data_8
        )

        a = bw.state.ParallelToSerialConverter8To1(
            enable,
            reset_n,
            parallel_load_n,
            data_bus,
            clock,
            output
        )

        enable.value = 1

        reset_n.value = 0
        assert output.value == 0
        reset_n.value = 1

        clock.value = 0
        parallel_load_n.value = 0
        data_1.value = 1
        data_2.value = 0
        data_3.value = 0
        data_4.value = 1
        data_5.value = 1
        data_6.value = 0
        data_7.value = 0
        data_8.value = 1
        clock.value = 1
        parallel_load_n.value = 1
        assert output.value == 1

        enable.value = 0

        clock.value = 0
        clock.value = 1
        assert output.value == 1

        enable.value = 1
        assert output.value == 1

        clock.value = 0
        clock.value = 1
        assert output.value == 0

        clock.value = 0
        clock.value = 1
        assert output.value == 0

        clock.value = 0
        clock.value = 1
        assert output.value == 1

        clock.value = 0
        clock.value = 1
        assert output.value == 1

        clock.value = 0
        clock.value = 1
        assert output.value == 0

        clock.value = 0
        clock.value = 1
        assert output.value == 0

        clock.value = 0
        clock.value = 1
        assert output.value == 1

        clock.value = 0
        clock.value = 1
        assert output.value == 0

        clock.value = 0
        parallel_load_n.value = 0
        data_1.value = 1
        data_2.value = 1
        data_3.value = 1
        data_4.value = 1
        data_5.value = 1
        data_6.value = 1
        data_7.value = 1
        data_8.value = 1
        clock.value = 1
        parallel_load_n.value = 1
        assert output.value == 1

        clock.value = 0
        clock.value = 1
        assert output.value == 1

        reset_n.value = 0
        assert output.value == 0

        print(a.__doc__)
        print(a)

        a(
            enable=1,
            clear_n=1,
            load_n=0,
            data_bus=(0, 1, 0, 1, 0, 1, 0, 1),
            clock=0,
            output=None
        )
        assert output.value == 0
        a(clock=1)
        assert output.value == 1
