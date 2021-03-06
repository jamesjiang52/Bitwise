import bitwise as bw


class TestSRLatch:
    def test_SRLatch(self):
        set_ = bw.wire.Wire()
        reset = bw.wire.Wire()
        output = bw.wire.Wire()
        output_not = bw.wire.Wire()

        a = bw.storage.SRLatch(set_, reset, output, output_not)

        set_.value = 0
        reset.value = 1
        assert output.value == 0
        assert output_not.value == 1

        set_.value = 0
        reset.value = 0
        assert output.value == 0
        assert output_not.value == 1

        set_.value = 1
        reset.value = 0
        assert output.value == 1
        assert output_not.value == 0

        set_.value = 0
        reset.value = 0
        assert output.value == 1
        assert output_not.value == 0

        set_.value = 0
        reset.value = 1
        assert output.value == 0
        assert output_not.value == 1

        set_.value = 0
        reset.value = 0
        assert output.value == 0
        assert output_not.value == 1

        print(a.__doc__)
        print(a)

        a(set=1, reset=0, output=None, output_not=None)
        assert output.value == 1
        assert output_not.value == 0
