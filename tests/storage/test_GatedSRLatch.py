import bitwise as bw


class TestGatedSRLatch:
    def test_GatedSRLatch(self):
        clock = bw.wire.Wire()
        set_ = bw.wire.Wire()
        reset = bw.wire.Wire()
        output = bw.wire.Wire()
        output_not = bw.wire.Wire()

        a = bw.storage.GatedSRLatch(set_, reset, clock, output, output_not)

        clock.value = 1
        set_.value = 0
        reset.value = 1
        assert output.value == 0
        assert output_not.value == 1

        clock.value = 1
        set_.value = 0
        reset.value = 0
        assert output.value == 0
        assert output_not.value == 1

        clock.value = 1
        set_.value = 1
        reset.value = 0
        assert output.value == 1
        assert output_not.value == 0

        clock.value = 1
        set_.value = 0
        reset.value = 0
        assert output.value == 1
        assert output_not.value == 0

        clock.value = 1
        set_.value = 0
        reset.value = 1
        assert output.value == 0
        assert output_not.value == 1

        clock.value = 0
        set_.value = 0
        reset.value = 1
        assert output.value == 0
        assert output_not.value == 1

        clock.value = 0
        set_.value = 0
        reset.value = 0
        assert output.value == 0
        assert output_not.value == 1

        clock.value = 0
        set_.value = 1
        reset.value = 0
        assert output.value == 0
        assert output_not.value == 1

        clock.value = 0
        set_.value = 0
        reset.value = 0
        assert output.value == 0
        assert output_not.value == 1

        clock.value = 0
        set_.value = 1
        reset.value = 0
        assert output.value == 0
        assert output_not.value == 1

        clock.value = 1
        set_.value = 1
        reset.value = 0
        assert output.value == 1
        assert output_not.value == 0

        clock.value = 0
        set_.value = 1
        reset.value = 0
        assert output.value == 1
        assert output_not.value == 0

        clock.value = 0
        set_.value = 0
        reset.value = 0
        assert output.value == 1
        assert output_not.value == 0

        clock.value = 0
        set_.value = 0
        reset.value = 1
        assert output.value == 1
        assert output_not.value == 0

        clock.value = 0
        set_.value = 0
        reset.value = 0
        assert output.value == 1
        assert output_not.value == 0

        clock.value = 0
        set_.value = 0
        reset.value = 1
        assert output.value == 1
        assert output_not.value == 0

        clock.value = 1
        set_.value = 0
        reset.value = 1
        assert output.value == 0
        assert output_not.value == 1

        clock.value = 1
        set_.value = 0
        reset.value = 0
        assert output.value == 0
        assert output_not.value == 1

        print(a.__doc__)
        print(a)
