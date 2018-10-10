import bitwise as bw


class TestHalfAdder:
    def test_HalfAdder(self):
        input_1 = bw.wire.Wire()
        input_2 = bw.wire.Wire()
        carry_out = bw.wire.Wire()
        sum_ = bw.wire.Wire()

        bw.arithmetic.HalfAdder(input_1, input_2, carry_out, sum_)

        input_1.value = 0
        input_2.value = 0
        assert (carry_out.value, sum_.value) == (0, 0)

        input_1.value = 0
        input_2.value = 1
        assert (carry_out.value, sum_.value) == (0, 1)

        input_1.value = 1
        input_2.value = 0
        assert (carry_out.value, sum_.value) == (0, 1)

        input_1.value = 1
        input_2.value = 1
        assert (carry_out.value, sum_.value) == (1, 0)
