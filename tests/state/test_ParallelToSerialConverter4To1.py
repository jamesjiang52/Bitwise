import bitwise as bw


class TestParallelToSerialConverter4To1:
    def test_ParallelToSerialConverter4To1(self):
        enable = bw.wire.Wire()
        reset_n = bw.wire.Wire()
        parallel_load_n = bw.wire.Wire()
        data_1 = bw.wire.Wire()
        data_2 = bw.wire.Wire()
        data_3 = bw.wire.Wire()
        data_4 = bw.wire.Wire()
        clock = bw.wire.Wire()
        output = bw.wire.Wire()
        data_bus = bw.wire.Bus4(data_1, data_2, data_3, data_4)

        bw.state.ParallelToSerialConverter4To1(
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
        assert output.value == 0

        clock.value = 0
        parallel_load_n.value = 0
        data_1.value = 1
        data_2.value = 1
        data_3.value = 1
        data_4.value = 1
        clock.value = 1
        parallel_load_n.value = 1
        assert output.value == 1

        clock.value = 0
        clock.value = 1
        assert output.value == 1

        reset_n.value = 0
        assert output.value == 0
