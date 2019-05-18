import bitwise as bw


class TestParityChecker4:
    def test_ParityChecker4(self):
        input_1 = bw.wire.Wire()
        input_2 = bw.wire.Wire()
        input_3 = bw.wire.Wire()
        input_4 = bw.wire.Wire()
        parity_bit = bw.wire.Wire()
        error = bw.wire.Wire()
        input_bus = bw.wire.Bus4(input_1, input_2, input_3, input_4)

        a = bw.logic.ParityChecker4(input_bus, parity_bit, error)

        input_1.value = 0
        input_2.value = 0
        input_3.value = 0
        input_4.value = 0
        parity_bit.value = 0
        assert error.value == 0

        input_1.value = 0
        input_2.value = 0
        input_3.value = 0
        input_4.value = 0
        parity_bit.value = 1
        assert error.value == 1

        input_1.value = 0
        input_2.value = 0
        input_3.value = 0
        input_4.value = 1
        parity_bit.value = 0
        assert error.value == 1

        input_1.value = 0
        input_2.value = 0
        input_3.value = 0
        input_4.value = 1
        parity_bit.value = 1
        assert error.value == 0

        input_1.value = 0
        input_2.value = 0
        input_3.value = 1
        input_4.value = 1
        parity_bit.value = 0
        assert error.value == 0

        input_1.value = 0
        input_2.value = 0
        input_3.value = 1
        input_4.value = 1
        parity_bit.value = 1
        assert error.value == 1

        input_1.value = 0
        input_2.value = 1
        input_3.value = 1
        input_4.value = 1
        parity_bit.value = 0
        assert error.value == 1

        input_1.value = 0
        input_2.value = 1
        input_3.value = 1
        input_4.value = 1
        parity_bit.value = 1
        assert error.value == 0

        input_1.value = 1
        input_2.value = 1
        input_3.value = 1
        input_4.value = 1
        parity_bit.value = 0
        assert error.value == 0

        input_1.value = 1
        input_2.value = 1
        input_3.value = 1
        input_4.value = 1
        parity_bit.value = 1
        assert error.value == 1

        print(a.__doc__)
        print(a)

        a(
            input_bus=(0, 1, 1, 1),
            parity_bit=0,
            error=None
        )
        assert error.value == 1
