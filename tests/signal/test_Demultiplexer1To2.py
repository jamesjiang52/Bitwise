import bitwise as bw


class TestDemultiplexer1To2:
    def test_Demultiplexer1To2(self):
        enable = bw.wire.Wire()
        select = bw.wire.Wire()
        input_ = bw.wire.Wire()
        output_1 = bw.wire.Wire()
        output_2 = bw.wire.Wire()

        a = bw.signal.Demultiplexer1To2(
            enable, select, input_, output_1, output_2
        )

        enable.value = 0
        select.value = 0
        input_.value = 0
        assert (output_1.value, output_2.value) == (0, 0)

        enable.value = 0
        select.value = 1
        input_.value = 1
        assert (output_1.value, output_2.value) == (0, 0)

        enable.value = 1
        select.value = 0
        input_.value = 0
        assert (output_1.value, output_2.value) == (0, 0)

        enable.value = 1
        select.value = 0
        input_.value = 1
        assert (output_1.value, output_2.value) == (0, 1)

        enable.value = 1
        select.value = 1
        input_.value = 0
        assert (output_1.value, output_2.value) == (0, 0)

        enable.value = 1
        select.value = 1
        input_.value = 1
        assert (output_1.value, output_2.value) == (1, 0)

        print(a.__doc__)
        print(a)
