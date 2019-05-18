import bitwise as bw


class TestParityGenerator16:
    def test_ParityGenerator16(self):
        input_1 = bw.wire.Wire()
        input_2 = bw.wire.Wire()
        input_3 = bw.wire.Wire()
        input_4 = bw.wire.Wire()
        input_5 = bw.wire.Wire()
        input_6 = bw.wire.Wire()
        input_7 = bw.wire.Wire()
        input_8 = bw.wire.Wire()
        input_9 = bw.wire.Wire()
        input_10 = bw.wire.Wire()
        input_11 = bw.wire.Wire()
        input_12 = bw.wire.Wire()
        input_13 = bw.wire.Wire()
        input_14 = bw.wire.Wire()
        input_15 = bw.wire.Wire()
        input_16 = bw.wire.Wire()
        parity_bit = bw.wire.Wire()
        input_bus = bw.wire.Bus16(
            input_1,
            input_2,
            input_3,
            input_4,
            input_5,
            input_6,
            input_7,
            input_8,
            input_9,
            input_10,
            input_11,
            input_12,
            input_13,
            input_14,
            input_15,
            input_16
        )

        a = bw.logic.ParityGenerator16(input_bus, parity_bit)

        input_1.value = 0
        input_2.value = 0
        input_3.value = 0
        input_4.value = 0
        input_5.value = 0
        input_6.value = 0
        input_7.value = 0
        input_8.value = 0
        input_9.value = 0
        input_10.value = 0
        input_11.value = 0
        input_12.value = 0
        input_13.value = 0
        input_14.value = 0
        input_15.value = 0
        input_16.value = 0
        assert parity_bit.value == 0

        input_1.value = 0
        input_2.value = 0
        input_3.value = 0
        input_4.value = 0
        input_5.value = 0
        input_6.value = 0
        input_7.value = 0
        input_8.value = 0
        input_9.value = 0
        input_10.value = 0
        input_11.value = 0
        input_12.value = 0
        input_13.value = 0
        input_14.value = 0
        input_15.value = 0
        input_16.value = 1
        assert parity_bit.value == 1

        input_1.value = 0
        input_2.value = 0
        input_3.value = 0
        input_4.value = 0
        input_5.value = 0
        input_6.value = 0
        input_7.value = 0
        input_8.value = 0
        input_9.value = 0
        input_10.value = 0
        input_11.value = 0
        input_12.value = 0
        input_13.value = 0
        input_14.value = 0
        input_15.value = 1
        input_16.value = 0
        assert parity_bit.value == 1

        input_1.value = 0
        input_2.value = 0
        input_3.value = 0
        input_4.value = 0
        input_5.value = 0
        input_6.value = 0
        input_7.value = 0
        input_8.value = 0
        input_9.value = 0
        input_10.value = 0
        input_11.value = 0
        input_12.value = 0
        input_13.value = 0
        input_14.value = 1
        input_15.value = 0
        input_16.value = 0
        assert parity_bit.value == 1

        input_1.value = 0
        input_2.value = 0
        input_3.value = 0
        input_4.value = 0
        input_5.value = 0
        input_6.value = 0
        input_7.value = 0
        input_8.value = 0
        input_9.value = 0
        input_10.value = 0
        input_11.value = 0
        input_12.value = 0
        input_13.value = 1
        input_14.value = 0
        input_15.value = 0
        input_16.value = 0
        assert parity_bit.value == 1

        input_1.value = 0
        input_2.value = 0
        input_3.value = 0
        input_4.value = 0
        input_5.value = 0
        input_6.value = 0
        input_7.value = 0
        input_8.value = 0
        input_9.value = 0
        input_10.value = 0
        input_11.value = 0
        input_12.value = 1
        input_13.value = 0
        input_14.value = 0
        input_15.value = 0
        input_16.value = 0
        assert parity_bit.value == 1

        input_1.value = 0
        input_2.value = 0
        input_3.value = 0
        input_4.value = 0
        input_5.value = 0
        input_6.value = 0
        input_7.value = 0
        input_8.value = 0
        input_9.value = 0
        input_10.value = 0
        input_11.value = 1
        input_12.value = 0
        input_13.value = 0
        input_14.value = 0
        input_15.value = 0
        input_16.value = 0
        assert parity_bit.value == 1

        input_1.value = 0
        input_2.value = 0
        input_3.value = 0
        input_4.value = 0
        input_5.value = 0
        input_6.value = 0
        input_7.value = 0
        input_8.value = 0
        input_9.value = 0
        input_10.value = 1
        input_11.value = 0
        input_12.value = 0
        input_13.value = 0
        input_14.value = 0
        input_15.value = 0
        input_16.value = 0
        assert parity_bit.value == 1

        input_1.value = 0
        input_2.value = 0
        input_3.value = 0
        input_4.value = 0
        input_5.value = 0
        input_6.value = 0
        input_7.value = 0
        input_8.value = 0
        input_9.value = 1
        input_10.value = 0
        input_11.value = 0
        input_12.value = 0
        input_13.value = 0
        input_14.value = 0
        input_15.value = 0
        input_16.value = 0
        assert parity_bit.value == 1

        input_1.value = 0
        input_2.value = 0
        input_3.value = 0
        input_4.value = 0
        input_5.value = 0
        input_6.value = 0
        input_7.value = 0
        input_8.value = 1
        input_9.value = 0
        input_10.value = 0
        input_11.value = 0
        input_12.value = 0
        input_13.value = 0
        input_14.value = 0
        input_15.value = 0
        input_16.value = 0
        assert parity_bit.value == 1

        input_1.value = 0
        input_2.value = 0
        input_3.value = 0
        input_4.value = 0
        input_5.value = 0
        input_6.value = 0
        input_7.value = 1
        input_8.value = 0
        input_9.value = 0
        input_10.value = 0
        input_11.value = 0
        input_12.value = 0
        input_13.value = 0
        input_14.value = 0
        input_15.value = 0
        input_16.value = 0
        assert parity_bit.value == 1

        input_1.value = 0
        input_2.value = 0
        input_3.value = 0
        input_4.value = 0
        input_5.value = 0
        input_6.value = 1
        input_7.value = 0
        input_8.value = 0
        input_9.value = 0
        input_10.value = 0
        input_11.value = 0
        input_12.value = 0
        input_13.value = 0
        input_14.value = 0
        input_15.value = 0
        input_16.value = 0
        assert parity_bit.value == 1

        input_1.value = 0
        input_2.value = 0
        input_3.value = 0
        input_4.value = 0
        input_5.value = 1
        input_6.value = 0
        input_7.value = 0
        input_8.value = 0
        input_9.value = 0
        input_10.value = 0
        input_11.value = 0
        input_12.value = 0
        input_13.value = 0
        input_14.value = 0
        input_15.value = 0
        input_16.value = 0
        assert parity_bit.value == 1

        input_1.value = 0
        input_2.value = 0
        input_3.value = 0
        input_4.value = 1
        input_5.value = 0
        input_6.value = 0
        input_7.value = 0
        input_8.value = 0
        input_9.value = 0
        input_10.value = 0
        input_11.value = 0
        input_12.value = 0
        input_13.value = 0
        input_14.value = 0
        input_15.value = 0
        input_16.value = 0
        assert parity_bit.value == 1

        input_1.value = 0
        input_2.value = 0
        input_3.value = 1
        input_4.value = 0
        input_5.value = 0
        input_6.value = 0
        input_7.value = 0
        input_8.value = 0
        input_9.value = 0
        input_10.value = 0
        input_11.value = 0
        input_12.value = 0
        input_13.value = 0
        input_14.value = 0
        input_15.value = 0
        input_16.value = 0
        assert parity_bit.value == 1

        input_1.value = 0
        input_2.value = 1
        input_3.value = 0
        input_4.value = 0
        input_5.value = 0
        input_6.value = 0
        input_7.value = 0
        input_8.value = 0
        input_9.value = 0
        input_10.value = 0
        input_11.value = 0
        input_12.value = 0
        input_13.value = 0
        input_14.value = 0
        input_15.value = 0
        input_16.value = 0
        assert parity_bit.value == 1

        input_1.value = 1
        input_2.value = 0
        input_3.value = 0
        input_4.value = 0
        input_5.value = 0
        input_6.value = 0
        input_7.value = 0
        input_8.value = 0
        input_9.value = 0
        input_10.value = 0
        input_11.value = 0
        input_12.value = 0
        input_13.value = 0
        input_14.value = 0
        input_15.value = 0
        input_16.value = 0
        assert parity_bit.value == 1

        input_1.value = 0
        input_2.value = 0
        input_3.value = 0
        input_4.value = 0
        input_5.value = 0
        input_6.value = 0
        input_7.value = 0
        input_8.value = 0
        input_9.value = 0
        input_10.value = 0
        input_11.value = 0
        input_12.value = 0
        input_13.value = 0
        input_14.value = 0
        input_15.value = 1
        input_16.value = 1
        assert parity_bit.value == 0

        input_1.value = 0
        input_2.value = 0
        input_3.value = 0
        input_4.value = 0
        input_5.value = 0
        input_6.value = 0
        input_7.value = 0
        input_8.value = 0
        input_9.value = 0
        input_10.value = 0
        input_11.value = 0
        input_12.value = 0
        input_13.value = 0
        input_14.value = 1
        input_15.value = 1
        input_16.value = 0
        assert parity_bit.value == 0

        input_1.value = 0
        input_2.value = 0
        input_3.value = 0
        input_4.value = 0
        input_5.value = 0
        input_6.value = 0
        input_7.value = 0
        input_8.value = 0
        input_9.value = 0
        input_10.value = 0
        input_11.value = 0
        input_12.value = 0
        input_13.value = 1
        input_14.value = 1
        input_15.value = 0
        input_16.value = 0
        assert parity_bit.value == 0

        input_1.value = 0
        input_2.value = 0
        input_3.value = 0
        input_4.value = 0
        input_5.value = 0
        input_6.value = 0
        input_7.value = 0
        input_8.value = 0
        input_9.value = 0
        input_10.value = 0
        input_11.value = 0
        input_12.value = 1
        input_13.value = 1
        input_14.value = 0
        input_15.value = 0
        input_16.value = 0
        assert parity_bit.value == 0

        input_1.value = 0
        input_2.value = 0
        input_3.value = 0
        input_4.value = 0
        input_5.value = 0
        input_6.value = 0
        input_7.value = 0
        input_8.value = 0
        input_9.value = 0
        input_10.value = 0
        input_11.value = 1
        input_12.value = 1
        input_13.value = 0
        input_14.value = 0
        input_15.value = 0
        input_16.value = 0
        assert parity_bit.value == 0

        input_1.value = 0
        input_2.value = 0
        input_3.value = 0
        input_4.value = 0
        input_5.value = 0
        input_6.value = 0
        input_7.value = 0
        input_8.value = 0
        input_9.value = 0
        input_10.value = 1
        input_11.value = 1
        input_12.value = 0
        input_13.value = 0
        input_14.value = 0
        input_15.value = 0
        input_16.value = 0
        assert parity_bit.value == 0

        input_1.value = 0
        input_2.value = 0
        input_3.value = 0
        input_4.value = 0
        input_5.value = 0
        input_6.value = 0
        input_7.value = 0
        input_8.value = 0
        input_9.value = 1
        input_10.value = 1
        input_11.value = 0
        input_12.value = 0
        input_13.value = 0
        input_14.value = 0
        input_15.value = 0
        input_16.value = 0
        assert parity_bit.value == 0

        input_1.value = 0
        input_2.value = 0
        input_3.value = 0
        input_4.value = 0
        input_5.value = 0
        input_6.value = 0
        input_7.value = 0
        input_8.value = 1
        input_9.value = 1
        input_10.value = 0
        input_11.value = 0
        input_12.value = 0
        input_13.value = 0
        input_14.value = 0
        input_15.value = 0
        input_16.value = 0
        assert parity_bit.value == 0

        input_1.value = 0
        input_2.value = 0
        input_3.value = 0
        input_4.value = 0
        input_5.value = 0
        input_6.value = 0
        input_7.value = 1
        input_8.value = 1
        input_9.value = 0
        input_10.value = 0
        input_11.value = 0
        input_12.value = 0
        input_13.value = 0
        input_14.value = 0
        input_15.value = 0
        input_16.value = 0
        assert parity_bit.value == 0

        input_1.value = 0
        input_2.value = 0
        input_3.value = 0
        input_4.value = 0
        input_5.value = 0
        input_6.value = 1
        input_7.value = 1
        input_8.value = 0
        input_9.value = 0
        input_10.value = 0
        input_11.value = 0
        input_12.value = 0
        input_13.value = 0
        input_14.value = 0
        input_15.value = 0
        input_16.value = 0
        assert parity_bit.value == 0

        input_1.value = 0
        input_2.value = 0
        input_3.value = 0
        input_4.value = 0
        input_5.value = 1
        input_6.value = 1
        input_7.value = 0
        input_8.value = 0
        input_9.value = 0
        input_10.value = 0
        input_11.value = 0
        input_12.value = 0
        input_13.value = 0
        input_14.value = 0
        input_15.value = 0
        input_16.value = 0
        assert parity_bit.value == 0

        input_1.value = 0
        input_2.value = 0
        input_3.value = 0
        input_4.value = 1
        input_5.value = 1
        input_6.value = 0
        input_7.value = 0
        input_8.value = 0
        input_9.value = 0
        input_10.value = 0
        input_11.value = 0
        input_12.value = 0
        input_13.value = 0
        input_14.value = 0
        input_15.value = 0
        input_16.value = 0
        assert parity_bit.value == 0

        input_1.value = 0
        input_2.value = 0
        input_3.value = 1
        input_4.value = 1
        input_5.value = 0
        input_6.value = 0
        input_7.value = 0
        input_8.value = 0
        input_9.value = 0
        input_10.value = 0
        input_11.value = 0
        input_12.value = 0
        input_13.value = 0
        input_14.value = 0
        input_15.value = 0
        input_16.value = 0
        assert parity_bit.value == 0

        input_1.value = 0
        input_2.value = 1
        input_3.value = 1
        input_4.value = 0
        input_5.value = 0
        input_6.value = 0
        input_7.value = 0
        input_8.value = 0
        input_9.value = 0
        input_10.value = 0
        input_11.value = 0
        input_12.value = 0
        input_13.value = 0
        input_14.value = 0
        input_15.value = 0
        input_16.value = 0
        assert parity_bit.value == 0

        input_1.value = 1
        input_2.value = 1
        input_3.value = 0
        input_4.value = 0
        input_5.value = 0
        input_6.value = 0
        input_7.value = 0
        input_8.value = 0
        input_9.value = 0
        input_10.value = 0
        input_11.value = 0
        input_12.value = 0
        input_13.value = 0
        input_14.value = 0
        input_15.value = 0
        input_16.value = 0
        assert parity_bit.value == 0

        input_1.value = 0
        input_2.value = 0
        input_3.value = 0
        input_4.value = 0
        input_5.value = 0
        input_6.value = 0
        input_7.value = 0
        input_8.value = 0
        input_9.value = 0
        input_10.value = 0
        input_11.value = 0
        input_12.value = 0
        input_13.value = 0
        input_14.value = 1
        input_15.value = 1
        input_16.value = 1
        assert parity_bit.value == 1

        input_1.value = 0
        input_2.value = 0
        input_3.value = 0
        input_4.value = 0
        input_5.value = 0
        input_6.value = 0
        input_7.value = 0
        input_8.value = 0
        input_9.value = 0
        input_10.value = 0
        input_11.value = 0
        input_12.value = 0
        input_13.value = 1
        input_14.value = 1
        input_15.value = 1
        input_16.value = 0
        assert parity_bit.value == 1

        input_1.value = 0
        input_2.value = 0
        input_3.value = 0
        input_4.value = 0
        input_5.value = 0
        input_6.value = 0
        input_7.value = 0
        input_8.value = 0
        input_9.value = 0
        input_10.value = 0
        input_11.value = 0
        input_12.value = 1
        input_13.value = 1
        input_14.value = 1
        input_15.value = 0
        input_16.value = 0
        assert parity_bit.value == 1

        input_1.value = 0
        input_2.value = 0
        input_3.value = 0
        input_4.value = 0
        input_5.value = 0
        input_6.value = 0
        input_7.value = 0
        input_8.value = 0
        input_9.value = 0
        input_10.value = 0
        input_11.value = 1
        input_12.value = 1
        input_13.value = 1
        input_14.value = 0
        input_15.value = 0
        input_16.value = 0
        assert parity_bit.value == 1

        input_1.value = 0
        input_2.value = 0
        input_3.value = 0
        input_4.value = 0
        input_5.value = 0
        input_6.value = 0
        input_7.value = 0
        input_8.value = 0
        input_9.value = 0
        input_10.value = 1
        input_11.value = 1
        input_12.value = 1
        input_13.value = 0
        input_14.value = 0
        input_15.value = 0
        input_16.value = 0
        assert parity_bit.value == 1

        input_1.value = 0
        input_2.value = 0
        input_3.value = 0
        input_4.value = 0
        input_5.value = 0
        input_6.value = 0
        input_7.value = 0
        input_8.value = 0
        input_9.value = 1
        input_10.value = 1
        input_11.value = 1
        input_12.value = 0
        input_13.value = 0
        input_14.value = 0
        input_15.value = 0
        input_16.value = 0
        assert parity_bit.value == 1

        input_1.value = 0
        input_2.value = 0
        input_3.value = 0
        input_4.value = 0
        input_5.value = 0
        input_6.value = 0
        input_7.value = 0
        input_8.value = 1
        input_9.value = 1
        input_10.value = 1
        input_11.value = 0
        input_12.value = 0
        input_13.value = 0
        input_14.value = 0
        input_15.value = 0
        input_16.value = 0
        assert parity_bit.value == 1

        input_1.value = 0
        input_2.value = 0
        input_3.value = 0
        input_4.value = 0
        input_5.value = 0
        input_6.value = 0
        input_7.value = 1
        input_8.value = 1
        input_9.value = 1
        input_10.value = 0
        input_11.value = 0
        input_12.value = 0
        input_13.value = 0
        input_14.value = 0
        input_15.value = 0
        input_16.value = 0
        assert parity_bit.value == 1

        input_1.value = 0
        input_2.value = 0
        input_3.value = 0
        input_4.value = 0
        input_5.value = 0
        input_6.value = 1
        input_7.value = 1
        input_8.value = 1
        input_9.value = 0
        input_10.value = 0
        input_11.value = 0
        input_12.value = 0
        input_13.value = 0
        input_14.value = 0
        input_15.value = 0
        input_16.value = 0
        assert parity_bit.value == 1

        input_1.value = 0
        input_2.value = 0
        input_3.value = 0
        input_4.value = 0
        input_5.value = 1
        input_6.value = 1
        input_7.value = 1
        input_8.value = 0
        input_9.value = 0
        input_10.value = 0
        input_11.value = 0
        input_12.value = 0
        input_13.value = 0
        input_14.value = 0
        input_15.value = 0
        input_16.value = 0
        assert parity_bit.value == 1

        input_1.value = 0
        input_2.value = 0
        input_3.value = 0
        input_4.value = 1
        input_5.value = 1
        input_6.value = 1
        input_7.value = 0
        input_8.value = 0
        input_9.value = 0
        input_10.value = 0
        input_11.value = 0
        input_12.value = 0
        input_13.value = 0
        input_14.value = 0
        input_15.value = 0
        input_16.value = 0
        assert parity_bit.value == 1

        input_1.value = 0
        input_2.value = 0
        input_3.value = 1
        input_4.value = 1
        input_5.value = 1
        input_6.value = 0
        input_7.value = 0
        input_8.value = 0
        input_9.value = 0
        input_10.value = 0
        input_11.value = 0
        input_12.value = 0
        input_13.value = 0
        input_14.value = 0
        input_15.value = 0
        input_16.value = 0
        assert parity_bit.value == 1

        input_1.value = 0
        input_2.value = 1
        input_3.value = 1
        input_4.value = 1
        input_5.value = 0
        input_6.value = 0
        input_7.value = 0
        input_8.value = 0
        input_9.value = 0
        input_10.value = 0
        input_11.value = 0
        input_12.value = 0
        input_13.value = 0
        input_14.value = 0
        input_15.value = 0
        input_16.value = 0
        assert parity_bit.value == 1

        input_1.value = 1
        input_2.value = 1
        input_3.value = 1
        input_4.value = 0
        input_5.value = 0
        input_6.value = 0
        input_7.value = 0
        input_8.value = 0
        input_9.value = 0
        input_10.value = 0
        input_11.value = 0
        input_12.value = 0
        input_13.value = 0
        input_14.value = 0
        input_15.value = 0
        input_16.value = 0
        assert parity_bit.value == 1

        input_1.value = 0
        input_2.value = 0
        input_3.value = 0
        input_4.value = 0
        input_5.value = 0
        input_6.value = 0
        input_7.value = 0
        input_8.value = 0
        input_9.value = 0
        input_10.value = 0
        input_11.value = 0
        input_12.value = 0
        input_13.value = 1
        input_14.value = 1
        input_15.value = 1
        input_16.value = 1
        assert parity_bit.value == 0

        input_1.value = 0
        input_2.value = 0
        input_3.value = 0
        input_4.value = 0
        input_5.value = 0
        input_6.value = 0
        input_7.value = 0
        input_8.value = 0
        input_9.value = 0
        input_10.value = 0
        input_11.value = 0
        input_12.value = 1
        input_13.value = 1
        input_14.value = 1
        input_15.value = 1
        input_16.value = 0
        assert parity_bit.value == 0

        input_1.value = 0
        input_2.value = 0
        input_3.value = 0
        input_4.value = 0
        input_5.value = 0
        input_6.value = 0
        input_7.value = 0
        input_8.value = 0
        input_9.value = 0
        input_10.value = 0
        input_11.value = 1
        input_12.value = 1
        input_13.value = 1
        input_14.value = 1
        input_15.value = 0
        input_16.value = 0
        assert parity_bit.value == 0

        input_1.value = 0
        input_2.value = 0
        input_3.value = 0
        input_4.value = 0
        input_5.value = 0
        input_6.value = 0
        input_7.value = 0
        input_8.value = 0
        input_9.value = 0
        input_10.value = 1
        input_11.value = 1
        input_12.value = 1
        input_13.value = 1
        input_14.value = 0
        input_15.value = 0
        input_16.value = 0
        assert parity_bit.value == 0

        input_1.value = 0
        input_2.value = 0
        input_3.value = 0
        input_4.value = 0
        input_5.value = 0
        input_6.value = 0
        input_7.value = 0
        input_8.value = 0
        input_9.value = 1
        input_10.value = 1
        input_11.value = 1
        input_12.value = 1
        input_13.value = 0
        input_14.value = 0
        input_15.value = 0
        input_16.value = 0
        assert parity_bit.value == 0

        input_1.value = 0
        input_2.value = 0
        input_3.value = 0
        input_4.value = 0
        input_5.value = 0
        input_6.value = 0
        input_7.value = 0
        input_8.value = 1
        input_9.value = 1
        input_10.value = 1
        input_11.value = 1
        input_12.value = 0
        input_13.value = 0
        input_14.value = 0
        input_15.value = 0
        input_16.value = 0
        assert parity_bit.value == 0

        input_1.value = 0
        input_2.value = 0
        input_3.value = 0
        input_4.value = 0
        input_5.value = 0
        input_6.value = 0
        input_7.value = 1
        input_8.value = 1
        input_9.value = 1
        input_10.value = 1
        input_11.value = 0
        input_12.value = 0
        input_13.value = 0
        input_14.value = 0
        input_15.value = 0
        input_16.value = 0
        assert parity_bit.value == 0

        input_1.value = 0
        input_2.value = 0
        input_3.value = 0
        input_4.value = 0
        input_5.value = 0
        input_6.value = 1
        input_7.value = 1
        input_8.value = 1
        input_9.value = 1
        input_10.value = 0
        input_11.value = 0
        input_12.value = 0
        input_13.value = 0
        input_14.value = 0
        input_15.value = 0
        input_16.value = 0
        assert parity_bit.value == 0

        input_1.value = 0
        input_2.value = 0
        input_3.value = 0
        input_4.value = 0
        input_5.value = 1
        input_6.value = 1
        input_7.value = 1
        input_8.value = 1
        input_9.value = 0
        input_10.value = 0
        input_11.value = 0
        input_12.value = 0
        input_13.value = 0
        input_14.value = 0
        input_15.value = 0
        input_16.value = 0
        assert parity_bit.value == 0

        input_1.value = 0
        input_2.value = 0
        input_3.value = 0
        input_4.value = 1
        input_5.value = 1
        input_6.value = 1
        input_7.value = 1
        input_8.value = 0
        input_9.value = 0
        input_10.value = 0
        input_11.value = 0
        input_12.value = 0
        input_13.value = 0
        input_14.value = 0
        input_15.value = 0
        input_16.value = 0
        assert parity_bit.value == 0

        input_1.value = 0
        input_2.value = 0
        input_3.value = 1
        input_4.value = 1
        input_5.value = 1
        input_6.value = 1
        input_7.value = 0
        input_8.value = 0
        input_9.value = 0
        input_10.value = 0
        input_11.value = 0
        input_12.value = 0
        input_13.value = 0
        input_14.value = 0
        input_15.value = 0
        input_16.value = 0
        assert parity_bit.value == 0

        input_1.value = 0
        input_2.value = 1
        input_3.value = 1
        input_4.value = 1
        input_5.value = 1
        input_6.value = 0
        input_7.value = 0
        input_8.value = 0
        input_9.value = 0
        input_10.value = 0
        input_11.value = 0
        input_12.value = 0
        input_13.value = 0
        input_14.value = 0
        input_15.value = 0
        input_16.value = 0
        assert parity_bit.value == 0

        input_1.value = 1
        input_2.value = 1
        input_3.value = 1
        input_4.value = 1
        input_5.value = 0
        input_6.value = 0
        input_7.value = 0
        input_8.value = 0
        input_9.value = 0
        input_10.value = 0
        input_11.value = 0
        input_12.value = 0
        input_13.value = 0
        input_14.value = 0
        input_15.value = 0
        input_16.value = 0
        assert parity_bit.value == 0

        input_1.value = 0
        input_2.value = 0
        input_3.value = 0
        input_4.value = 0
        input_5.value = 0
        input_6.value = 0
        input_7.value = 0
        input_8.value = 0
        input_9.value = 0
        input_10.value = 0
        input_11.value = 0
        input_12.value = 1
        input_13.value = 1
        input_14.value = 1
        input_15.value = 1
        input_16.value = 1
        assert parity_bit.value == 1

        input_1.value = 0
        input_2.value = 0
        input_3.value = 0
        input_4.value = 0
        input_5.value = 0
        input_6.value = 0
        input_7.value = 0
        input_8.value = 0
        input_9.value = 0
        input_10.value = 0
        input_11.value = 1
        input_12.value = 1
        input_13.value = 1
        input_14.value = 1
        input_15.value = 1
        input_16.value = 0
        assert parity_bit.value == 1

        input_1.value = 0
        input_2.value = 0
        input_3.value = 0
        input_4.value = 0
        input_5.value = 0
        input_6.value = 0
        input_7.value = 0
        input_8.value = 0
        input_9.value = 0
        input_10.value = 1
        input_11.value = 1
        input_12.value = 1
        input_13.value = 1
        input_14.value = 1
        input_15.value = 0
        input_16.value = 0
        assert parity_bit.value == 1

        input_1.value = 0
        input_2.value = 0
        input_3.value = 0
        input_4.value = 0
        input_5.value = 0
        input_6.value = 0
        input_7.value = 0
        input_8.value = 0
        input_9.value = 1
        input_10.value = 1
        input_11.value = 1
        input_12.value = 1
        input_13.value = 1
        input_14.value = 0
        input_15.value = 0
        input_16.value = 0
        assert parity_bit.value == 1

        input_1.value = 0
        input_2.value = 0
        input_3.value = 0
        input_4.value = 0
        input_5.value = 0
        input_6.value = 0
        input_7.value = 0
        input_8.value = 1
        input_9.value = 1
        input_10.value = 1
        input_11.value = 1
        input_12.value = 1
        input_13.value = 0
        input_14.value = 0
        input_15.value = 0
        input_16.value = 0
        assert parity_bit.value == 1

        input_1.value = 0
        input_2.value = 0
        input_3.value = 0
        input_4.value = 0
        input_5.value = 0
        input_6.value = 0
        input_7.value = 1
        input_8.value = 1
        input_9.value = 1
        input_10.value = 1
        input_11.value = 1
        input_12.value = 0
        input_13.value = 0
        input_14.value = 0
        input_15.value = 0
        input_16.value = 0
        assert parity_bit.value == 1

        input_1.value = 0
        input_2.value = 0
        input_3.value = 0
        input_4.value = 0
        input_5.value = 0
        input_6.value = 1
        input_7.value = 1
        input_8.value = 1
        input_9.value = 1
        input_10.value = 1
        input_11.value = 0
        input_12.value = 0
        input_13.value = 0
        input_14.value = 0
        input_15.value = 0
        input_16.value = 0
        assert parity_bit.value == 1

        input_1.value = 0
        input_2.value = 0
        input_3.value = 0
        input_4.value = 0
        input_5.value = 1
        input_6.value = 1
        input_7.value = 1
        input_8.value = 1
        input_9.value = 1
        input_10.value = 0
        input_11.value = 0
        input_12.value = 0
        input_13.value = 0
        input_14.value = 0
        input_15.value = 0
        input_16.value = 0
        assert parity_bit.value == 1

        input_1.value = 0
        input_2.value = 0
        input_3.value = 0
        input_4.value = 1
        input_5.value = 1
        input_6.value = 1
        input_7.value = 1
        input_8.value = 1
        input_9.value = 0
        input_10.value = 0
        input_11.value = 0
        input_12.value = 0
        input_13.value = 0
        input_14.value = 0
        input_15.value = 0
        input_16.value = 0
        assert parity_bit.value == 1

        input_1.value = 0
        input_2.value = 0
        input_3.value = 1
        input_4.value = 1
        input_5.value = 1
        input_6.value = 1
        input_7.value = 1
        input_8.value = 0
        input_9.value = 0
        input_10.value = 0
        input_11.value = 0
        input_12.value = 0
        input_13.value = 0
        input_14.value = 0
        input_15.value = 0
        input_16.value = 0
        assert parity_bit.value == 1

        input_1.value = 0
        input_2.value = 1
        input_3.value = 1
        input_4.value = 1
        input_5.value = 1
        input_6.value = 1
        input_7.value = 0
        input_8.value = 0
        input_9.value = 0
        input_10.value = 0
        input_11.value = 0
        input_12.value = 0
        input_13.value = 0
        input_14.value = 0
        input_15.value = 0
        input_16.value = 0
        assert parity_bit.value == 1

        input_1.value = 1
        input_2.value = 1
        input_3.value = 1
        input_4.value = 1
        input_5.value = 1
        input_6.value = 0
        input_7.value = 0
        input_8.value = 0
        input_9.value = 0
        input_10.value = 0
        input_11.value = 0
        input_12.value = 0
        input_13.value = 0
        input_14.value = 0
        input_15.value = 0
        input_16.value = 0
        assert parity_bit.value == 1

        input_1.value = 0
        input_2.value = 0
        input_3.value = 0
        input_4.value = 0
        input_5.value = 0
        input_6.value = 0
        input_7.value = 0
        input_8.value = 0
        input_9.value = 0
        input_10.value = 0
        input_11.value = 1
        input_12.value = 1
        input_13.value = 1
        input_14.value = 1
        input_15.value = 1
        input_16.value = 1
        assert parity_bit.value == 0

        input_1.value = 0
        input_2.value = 0
        input_3.value = 0
        input_4.value = 0
        input_5.value = 0
        input_6.value = 0
        input_7.value = 0
        input_8.value = 0
        input_9.value = 0
        input_10.value = 1
        input_11.value = 1
        input_12.value = 1
        input_13.value = 1
        input_14.value = 1
        input_15.value = 1
        input_16.value = 0
        assert parity_bit.value == 0

        input_1.value = 0
        input_2.value = 0
        input_3.value = 0
        input_4.value = 0
        input_5.value = 0
        input_6.value = 0
        input_7.value = 0
        input_8.value = 0
        input_9.value = 1
        input_10.value = 1
        input_11.value = 1
        input_12.value = 1
        input_13.value = 1
        input_14.value = 1
        input_15.value = 0
        input_16.value = 0
        assert parity_bit.value == 0

        input_1.value = 0
        input_2.value = 0
        input_3.value = 0
        input_4.value = 0
        input_5.value = 0
        input_6.value = 0
        input_7.value = 0
        input_8.value = 1
        input_9.value = 1
        input_10.value = 1
        input_11.value = 1
        input_12.value = 1
        input_13.value = 1
        input_14.value = 0
        input_15.value = 0
        input_16.value = 0
        assert parity_bit.value == 0

        input_1.value = 0
        input_2.value = 0
        input_3.value = 0
        input_4.value = 0
        input_5.value = 0
        input_6.value = 0
        input_7.value = 1
        input_8.value = 1
        input_9.value = 1
        input_10.value = 1
        input_11.value = 1
        input_12.value = 1
        input_13.value = 0
        input_14.value = 0
        input_15.value = 0
        input_16.value = 0
        assert parity_bit.value == 0

        input_1.value = 0
        input_2.value = 0
        input_3.value = 0
        input_4.value = 0
        input_5.value = 0
        input_6.value = 1
        input_7.value = 1
        input_8.value = 1
        input_9.value = 1
        input_10.value = 1
        input_11.value = 1
        input_12.value = 0
        input_13.value = 0
        input_14.value = 0
        input_15.value = 0
        input_16.value = 0
        assert parity_bit.value == 0

        input_1.value = 0
        input_2.value = 0
        input_3.value = 0
        input_4.value = 0
        input_5.value = 1
        input_6.value = 1
        input_7.value = 1
        input_8.value = 1
        input_9.value = 1
        input_10.value = 1
        input_11.value = 0
        input_12.value = 0
        input_13.value = 0
        input_14.value = 0
        input_15.value = 0
        input_16.value = 0
        assert parity_bit.value == 0

        input_1.value = 0
        input_2.value = 0
        input_3.value = 0
        input_4.value = 1
        input_5.value = 1
        input_6.value = 1
        input_7.value = 1
        input_8.value = 1
        input_9.value = 1
        input_10.value = 0
        input_11.value = 0
        input_12.value = 0
        input_13.value = 0
        input_14.value = 0
        input_15.value = 0
        input_16.value = 0
        assert parity_bit.value == 0

        input_1.value = 0
        input_2.value = 0
        input_3.value = 1
        input_4.value = 1
        input_5.value = 1
        input_6.value = 1
        input_7.value = 1
        input_8.value = 1
        input_9.value = 0
        input_10.value = 0
        input_11.value = 0
        input_12.value = 0
        input_13.value = 0
        input_14.value = 0
        input_15.value = 0
        input_16.value = 0
        assert parity_bit.value == 0

        input_1.value = 0
        input_2.value = 1
        input_3.value = 1
        input_4.value = 1
        input_5.value = 1
        input_6.value = 1
        input_7.value = 1
        input_8.value = 0
        input_9.value = 0
        input_10.value = 0
        input_11.value = 0
        input_12.value = 0
        input_13.value = 0
        input_14.value = 0
        input_15.value = 0
        input_16.value = 0
        assert parity_bit.value == 0

        input_1.value = 1
        input_2.value = 1
        input_3.value = 1
        input_4.value = 1
        input_5.value = 1
        input_6.value = 1
        input_7.value = 0
        input_8.value = 0
        input_9.value = 0
        input_10.value = 0
        input_11.value = 0
        input_12.value = 0
        input_13.value = 0
        input_14.value = 0
        input_15.value = 0
        input_16.value = 0
        assert parity_bit.value == 0

        input_1.value = 0
        input_2.value = 0
        input_3.value = 0
        input_4.value = 0
        input_5.value = 0
        input_6.value = 0
        input_7.value = 0
        input_8.value = 0
        input_9.value = 0
        input_10.value = 1
        input_11.value = 1
        input_12.value = 1
        input_13.value = 1
        input_14.value = 1
        input_15.value = 1
        input_16.value = 1
        assert parity_bit.value == 1

        input_1.value = 0
        input_2.value = 0
        input_3.value = 0
        input_4.value = 0
        input_5.value = 0
        input_6.value = 0
        input_7.value = 0
        input_8.value = 0
        input_9.value = 1
        input_10.value = 1
        input_11.value = 1
        input_12.value = 1
        input_13.value = 1
        input_14.value = 1
        input_15.value = 1
        input_16.value = 0
        assert parity_bit.value == 1

        input_1.value = 0
        input_2.value = 0
        input_3.value = 0
        input_4.value = 0
        input_5.value = 0
        input_6.value = 0
        input_7.value = 0
        input_8.value = 1
        input_9.value = 1
        input_10.value = 1
        input_11.value = 1
        input_12.value = 1
        input_13.value = 1
        input_14.value = 1
        input_15.value = 0
        input_16.value = 0
        assert parity_bit.value == 1

        input_1.value = 0
        input_2.value = 0
        input_3.value = 0
        input_4.value = 0
        input_5.value = 0
        input_6.value = 0
        input_7.value = 1
        input_8.value = 1
        input_9.value = 1
        input_10.value = 1
        input_11.value = 1
        input_12.value = 1
        input_13.value = 1
        input_14.value = 0
        input_15.value = 0
        input_16.value = 0
        assert parity_bit.value == 1

        input_1.value = 0
        input_2.value = 0
        input_3.value = 0
        input_4.value = 0
        input_5.value = 0
        input_6.value = 1
        input_7.value = 1
        input_8.value = 1
        input_9.value = 1
        input_10.value = 1
        input_11.value = 1
        input_12.value = 1
        input_13.value = 0
        input_14.value = 0
        input_15.value = 0
        input_16.value = 0
        assert parity_bit.value == 1

        input_1.value = 0
        input_2.value = 0
        input_3.value = 0
        input_4.value = 0
        input_5.value = 1
        input_6.value = 1
        input_7.value = 1
        input_8.value = 1
        input_9.value = 1
        input_10.value = 1
        input_11.value = 1
        input_12.value = 0
        input_13.value = 0
        input_14.value = 0
        input_15.value = 0
        input_16.value = 0
        assert parity_bit.value == 1

        input_1.value = 0
        input_2.value = 0
        input_3.value = 0
        input_4.value = 1
        input_5.value = 1
        input_6.value = 1
        input_7.value = 1
        input_8.value = 1
        input_9.value = 1
        input_10.value = 1
        input_11.value = 0
        input_12.value = 0
        input_13.value = 0
        input_14.value = 0
        input_15.value = 0
        input_16.value = 0
        assert parity_bit.value == 1

        input_1.value = 0
        input_2.value = 0
        input_3.value = 1
        input_4.value = 1
        input_5.value = 1
        input_6.value = 1
        input_7.value = 1
        input_8.value = 1
        input_9.value = 1
        input_10.value = 0
        input_11.value = 0
        input_12.value = 0
        input_13.value = 0
        input_14.value = 0
        input_15.value = 0
        input_16.value = 0
        assert parity_bit.value == 1

        input_1.value = 0
        input_2.value = 1
        input_3.value = 1
        input_4.value = 1
        input_5.value = 1
        input_6.value = 1
        input_7.value = 1
        input_8.value = 1
        input_9.value = 0
        input_10.value = 0
        input_11.value = 0
        input_12.value = 0
        input_13.value = 0
        input_14.value = 0
        input_15.value = 0
        input_16.value = 0
        assert parity_bit.value == 1

        input_1.value = 1
        input_2.value = 1
        input_3.value = 1
        input_4.value = 1
        input_5.value = 1
        input_6.value = 1
        input_7.value = 1
        input_8.value = 0
        input_9.value = 0
        input_10.value = 0
        input_11.value = 0
        input_12.value = 0
        input_13.value = 0
        input_14.value = 0
        input_15.value = 0
        input_16.value = 0
        assert parity_bit.value == 1

        input_1.value = 0
        input_2.value = 0
        input_3.value = 0
        input_4.value = 0
        input_5.value = 0
        input_6.value = 0
        input_7.value = 0
        input_8.value = 0
        input_9.value = 1
        input_10.value = 1
        input_11.value = 1
        input_12.value = 1
        input_13.value = 1
        input_14.value = 1
        input_15.value = 1
        input_16.value = 1
        assert parity_bit.value == 0

        input_1.value = 0
        input_2.value = 0
        input_3.value = 0
        input_4.value = 0
        input_5.value = 0
        input_6.value = 0
        input_7.value = 0
        input_8.value = 1
        input_9.value = 1
        input_10.value = 1
        input_11.value = 1
        input_12.value = 1
        input_13.value = 1
        input_14.value = 1
        input_15.value = 1
        input_16.value = 0
        assert parity_bit.value == 0

        input_1.value = 0
        input_2.value = 0
        input_3.value = 0
        input_4.value = 0
        input_5.value = 0
        input_6.value = 0
        input_7.value = 1
        input_8.value = 1
        input_9.value = 1
        input_10.value = 1
        input_11.value = 1
        input_12.value = 1
        input_13.value = 1
        input_14.value = 1
        input_15.value = 0
        input_16.value = 0
        assert parity_bit.value == 0

        input_1.value = 0
        input_2.value = 0
        input_3.value = 0
        input_4.value = 0
        input_5.value = 0
        input_6.value = 1
        input_7.value = 1
        input_8.value = 1
        input_9.value = 1
        input_10.value = 1
        input_11.value = 1
        input_12.value = 1
        input_13.value = 1
        input_14.value = 0
        input_15.value = 0
        input_16.value = 0
        assert parity_bit.value == 0

        input_1.value = 0
        input_2.value = 0
        input_3.value = 0
        input_4.value = 0
        input_5.value = 1
        input_6.value = 1
        input_7.value = 1
        input_8.value = 1
        input_9.value = 1
        input_10.value = 1
        input_11.value = 1
        input_12.value = 1
        input_13.value = 0
        input_14.value = 0
        input_15.value = 0
        input_16.value = 0
        assert parity_bit.value == 0

        input_1.value = 0
        input_2.value = 0
        input_3.value = 0
        input_4.value = 1
        input_5.value = 1
        input_6.value = 1
        input_7.value = 1
        input_8.value = 1
        input_9.value = 1
        input_10.value = 1
        input_11.value = 1
        input_12.value = 0
        input_13.value = 0
        input_14.value = 0
        input_15.value = 0
        input_16.value = 0
        assert parity_bit.value == 0

        input_1.value = 0
        input_2.value = 0
        input_3.value = 1
        input_4.value = 1
        input_5.value = 1
        input_6.value = 1
        input_7.value = 1
        input_8.value = 1
        input_9.value = 1
        input_10.value = 1
        input_11.value = 0
        input_12.value = 0
        input_13.value = 0
        input_14.value = 0
        input_15.value = 0
        input_16.value = 0
        assert parity_bit.value == 0

        input_1.value = 0
        input_2.value = 1
        input_3.value = 1
        input_4.value = 1
        input_5.value = 1
        input_6.value = 1
        input_7.value = 1
        input_8.value = 1
        input_9.value = 1
        input_10.value = 0
        input_11.value = 0
        input_12.value = 0
        input_13.value = 0
        input_14.value = 0
        input_15.value = 0
        input_16.value = 0
        assert parity_bit.value == 0

        input_1.value = 1
        input_2.value = 1
        input_3.value = 1
        input_4.value = 1
        input_5.value = 1
        input_6.value = 1
        input_7.value = 1
        input_8.value = 1
        input_9.value = 0
        input_10.value = 0
        input_11.value = 0
        input_12.value = 0
        input_13.value = 0
        input_14.value = 0
        input_15.value = 0
        input_16.value = 0
        assert parity_bit.value == 0

        input_1.value = 0
        input_2.value = 0
        input_3.value = 0
        input_4.value = 0
        input_5.value = 0
        input_6.value = 0
        input_7.value = 0
        input_8.value = 1
        input_9.value = 1
        input_10.value = 1
        input_11.value = 1
        input_12.value = 1
        input_13.value = 1
        input_14.value = 1
        input_15.value = 1
        input_16.value = 1
        assert parity_bit.value == 1

        input_1.value = 0
        input_2.value = 0
        input_3.value = 0
        input_4.value = 0
        input_5.value = 0
        input_6.value = 0
        input_7.value = 1
        input_8.value = 1
        input_9.value = 1
        input_10.value = 1
        input_11.value = 1
        input_12.value = 1
        input_13.value = 1
        input_14.value = 1
        input_15.value = 1
        input_16.value = 0
        assert parity_bit.value == 1

        input_1.value = 0
        input_2.value = 0
        input_3.value = 0
        input_4.value = 0
        input_5.value = 0
        input_6.value = 1
        input_7.value = 1
        input_8.value = 1
        input_9.value = 1
        input_10.value = 1
        input_11.value = 1
        input_12.value = 1
        input_13.value = 1
        input_14.value = 1
        input_15.value = 0
        input_16.value = 0
        assert parity_bit.value == 1

        input_1.value = 0
        input_2.value = 0
        input_3.value = 0
        input_4.value = 0
        input_5.value = 1
        input_6.value = 1
        input_7.value = 1
        input_8.value = 1
        input_9.value = 1
        input_10.value = 1
        input_11.value = 1
        input_12.value = 1
        input_13.value = 1
        input_14.value = 0
        input_15.value = 0
        input_16.value = 0
        assert parity_bit.value == 1

        input_1.value = 0
        input_2.value = 0
        input_3.value = 0
        input_4.value = 1
        input_5.value = 1
        input_6.value = 1
        input_7.value = 1
        input_8.value = 1
        input_9.value = 1
        input_10.value = 1
        input_11.value = 1
        input_12.value = 1
        input_13.value = 0
        input_14.value = 0
        input_15.value = 0
        input_16.value = 0
        assert parity_bit.value == 1

        input_1.value = 0
        input_2.value = 0
        input_3.value = 1
        input_4.value = 1
        input_5.value = 1
        input_6.value = 1
        input_7.value = 1
        input_8.value = 1
        input_9.value = 1
        input_10.value = 1
        input_11.value = 1
        input_12.value = 0
        input_13.value = 0
        input_14.value = 0
        input_15.value = 0
        input_16.value = 0
        assert parity_bit.value == 1

        input_1.value = 0
        input_2.value = 1
        input_3.value = 1
        input_4.value = 1
        input_5.value = 1
        input_6.value = 1
        input_7.value = 1
        input_8.value = 1
        input_9.value = 1
        input_10.value = 1
        input_11.value = 0
        input_12.value = 0
        input_13.value = 0
        input_14.value = 0
        input_15.value = 0
        input_16.value = 0
        assert parity_bit.value == 1

        input_1.value = 1
        input_2.value = 1
        input_3.value = 1
        input_4.value = 1
        input_5.value = 1
        input_6.value = 1
        input_7.value = 1
        input_8.value = 1
        input_9.value = 1
        input_10.value = 0
        input_11.value = 0
        input_12.value = 0
        input_13.value = 0
        input_14.value = 0
        input_15.value = 0
        input_16.value = 0
        assert parity_bit.value == 1

        input_1.value = 0
        input_2.value = 0
        input_3.value = 0
        input_4.value = 0
        input_5.value = 0
        input_6.value = 0
        input_7.value = 1
        input_8.value = 1
        input_9.value = 1
        input_10.value = 1
        input_11.value = 1
        input_12.value = 1
        input_13.value = 1
        input_14.value = 1
        input_15.value = 1
        input_16.value = 1
        assert parity_bit.value == 0

        input_1.value = 0
        input_2.value = 0
        input_3.value = 0
        input_4.value = 0
        input_5.value = 0
        input_6.value = 1
        input_7.value = 1
        input_8.value = 1
        input_9.value = 1
        input_10.value = 1
        input_11.value = 1
        input_12.value = 1
        input_13.value = 1
        input_14.value = 1
        input_15.value = 1
        input_16.value = 0
        assert parity_bit.value == 0

        input_1.value = 0
        input_2.value = 0
        input_3.value = 0
        input_4.value = 0
        input_5.value = 1
        input_6.value = 1
        input_7.value = 1
        input_8.value = 1
        input_9.value = 1
        input_10.value = 1
        input_11.value = 1
        input_12.value = 1
        input_13.value = 1
        input_14.value = 1
        input_15.value = 0
        input_16.value = 0
        assert parity_bit.value == 0

        input_1.value = 0
        input_2.value = 0
        input_3.value = 0
        input_4.value = 1
        input_5.value = 1
        input_6.value = 1
        input_7.value = 1
        input_8.value = 1
        input_9.value = 1
        input_10.value = 1
        input_11.value = 1
        input_12.value = 1
        input_13.value = 1
        input_14.value = 0
        input_15.value = 0
        input_16.value = 0
        assert parity_bit.value == 0

        input_1.value = 0
        input_2.value = 0
        input_3.value = 1
        input_4.value = 1
        input_5.value = 1
        input_6.value = 1
        input_7.value = 1
        input_8.value = 1
        input_9.value = 1
        input_10.value = 1
        input_11.value = 1
        input_12.value = 1
        input_13.value = 0
        input_14.value = 0
        input_15.value = 0
        input_16.value = 0
        assert parity_bit.value == 0

        input_1.value = 0
        input_2.value = 1
        input_3.value = 1
        input_4.value = 1
        input_5.value = 1
        input_6.value = 1
        input_7.value = 1
        input_8.value = 1
        input_9.value = 1
        input_10.value = 1
        input_11.value = 1
        input_12.value = 0
        input_13.value = 0
        input_14.value = 0
        input_15.value = 0
        input_16.value = 0
        assert parity_bit.value == 0

        input_1.value = 1
        input_2.value = 1
        input_3.value = 1
        input_4.value = 1
        input_5.value = 1
        input_6.value = 1
        input_7.value = 1
        input_8.value = 1
        input_9.value = 1
        input_10.value = 1
        input_11.value = 0
        input_12.value = 0
        input_13.value = 0
        input_14.value = 0
        input_15.value = 0
        input_16.value = 0
        assert parity_bit.value == 0

        input_1.value = 0
        input_2.value = 0
        input_3.value = 0
        input_4.value = 0
        input_5.value = 0
        input_6.value = 1
        input_7.value = 1
        input_8.value = 1
        input_9.value = 1
        input_10.value = 1
        input_11.value = 1
        input_12.value = 1
        input_13.value = 1
        input_14.value = 1
        input_15.value = 1
        input_16.value = 1
        assert parity_bit.value == 1

        input_1.value = 0
        input_2.value = 0
        input_3.value = 0
        input_4.value = 0
        input_5.value = 1
        input_6.value = 1
        input_7.value = 1
        input_8.value = 1
        input_9.value = 1
        input_10.value = 1
        input_11.value = 1
        input_12.value = 1
        input_13.value = 1
        input_14.value = 1
        input_15.value = 1
        input_16.value = 0
        assert parity_bit.value == 1

        input_1.value = 0
        input_2.value = 0
        input_3.value = 0
        input_4.value = 1
        input_5.value = 1
        input_6.value = 1
        input_7.value = 1
        input_8.value = 1
        input_9.value = 1
        input_10.value = 1
        input_11.value = 1
        input_12.value = 1
        input_13.value = 1
        input_14.value = 1
        input_15.value = 0
        input_16.value = 0
        assert parity_bit.value == 1

        input_1.value = 0
        input_2.value = 0
        input_3.value = 1
        input_4.value = 1
        input_5.value = 1
        input_6.value = 1
        input_7.value = 1
        input_8.value = 1
        input_9.value = 1
        input_10.value = 1
        input_11.value = 1
        input_12.value = 1
        input_13.value = 1
        input_14.value = 0
        input_15.value = 0
        input_16.value = 0
        assert parity_bit.value == 1

        input_1.value = 0
        input_2.value = 1
        input_3.value = 1
        input_4.value = 1
        input_5.value = 1
        input_6.value = 1
        input_7.value = 1
        input_8.value = 1
        input_9.value = 1
        input_10.value = 1
        input_11.value = 1
        input_12.value = 1
        input_13.value = 0
        input_14.value = 0
        input_15.value = 0
        input_16.value = 0
        assert parity_bit.value == 1

        input_1.value = 1
        input_2.value = 1
        input_3.value = 1
        input_4.value = 1
        input_5.value = 1
        input_6.value = 1
        input_7.value = 1
        input_8.value = 1
        input_9.value = 1
        input_10.value = 1
        input_11.value = 1
        input_12.value = 0
        input_13.value = 0
        input_14.value = 0
        input_15.value = 0
        input_16.value = 0
        assert parity_bit.value == 1

        input_1.value = 0
        input_2.value = 0
        input_3.value = 0
        input_4.value = 0
        input_5.value = 1
        input_6.value = 1
        input_7.value = 1
        input_8.value = 1
        input_9.value = 1
        input_10.value = 1
        input_11.value = 1
        input_12.value = 1
        input_13.value = 1
        input_14.value = 1
        input_15.value = 1
        input_16.value = 1
        assert parity_bit.value == 0

        input_1.value = 0
        input_2.value = 0
        input_3.value = 0
        input_4.value = 1
        input_5.value = 1
        input_6.value = 1
        input_7.value = 1
        input_8.value = 1
        input_9.value = 1
        input_10.value = 1
        input_11.value = 1
        input_12.value = 1
        input_13.value = 1
        input_14.value = 1
        input_15.value = 1
        input_16.value = 0
        assert parity_bit.value == 0

        input_1.value = 0
        input_2.value = 0
        input_3.value = 1
        input_4.value = 1
        input_5.value = 1
        input_6.value = 1
        input_7.value = 1
        input_8.value = 1
        input_9.value = 1
        input_10.value = 1
        input_11.value = 1
        input_12.value = 1
        input_13.value = 1
        input_14.value = 1
        input_15.value = 0
        input_16.value = 0
        assert parity_bit.value == 0

        input_1.value = 0
        input_2.value = 1
        input_3.value = 1
        input_4.value = 1
        input_5.value = 1
        input_6.value = 1
        input_7.value = 1
        input_8.value = 1
        input_9.value = 1
        input_10.value = 1
        input_11.value = 1
        input_12.value = 1
        input_13.value = 1
        input_14.value = 0
        input_15.value = 0
        input_16.value = 0
        assert parity_bit.value == 0

        input_1.value = 1
        input_2.value = 1
        input_3.value = 1
        input_4.value = 1
        input_5.value = 1
        input_6.value = 1
        input_7.value = 1
        input_8.value = 1
        input_9.value = 1
        input_10.value = 1
        input_11.value = 1
        input_12.value = 1
        input_13.value = 0
        input_14.value = 0
        input_15.value = 0
        input_16.value = 0
        assert parity_bit.value == 0

        input_1.value = 0
        input_2.value = 0
        input_3.value = 0
        input_4.value = 1
        input_5.value = 1
        input_6.value = 1
        input_7.value = 1
        input_8.value = 1
        input_9.value = 1
        input_10.value = 1
        input_11.value = 1
        input_12.value = 1
        input_13.value = 1
        input_14.value = 1
        input_15.value = 1
        input_16.value = 1
        assert parity_bit.value == 1

        input_1.value = 0
        input_2.value = 0
        input_3.value = 1
        input_4.value = 1
        input_5.value = 1
        input_6.value = 1
        input_7.value = 1
        input_8.value = 1
        input_9.value = 1
        input_10.value = 1
        input_11.value = 1
        input_12.value = 1
        input_13.value = 1
        input_14.value = 1
        input_15.value = 1
        input_16.value = 0
        assert parity_bit.value == 1

        input_1.value = 0
        input_2.value = 1
        input_3.value = 1
        input_4.value = 1
        input_5.value = 1
        input_6.value = 1
        input_7.value = 1
        input_8.value = 1
        input_9.value = 1
        input_10.value = 1
        input_11.value = 1
        input_12.value = 1
        input_13.value = 1
        input_14.value = 1
        input_15.value = 0
        input_16.value = 0
        assert parity_bit.value == 1

        input_1.value = 1
        input_2.value = 1
        input_3.value = 1
        input_4.value = 1
        input_5.value = 1
        input_6.value = 1
        input_7.value = 1
        input_8.value = 1
        input_9.value = 1
        input_10.value = 1
        input_11.value = 1
        input_12.value = 1
        input_13.value = 1
        input_14.value = 0
        input_15.value = 0
        input_16.value = 0
        assert parity_bit.value == 1

        input_1.value = 0
        input_2.value = 0
        input_3.value = 1
        input_4.value = 1
        input_5.value = 1
        input_6.value = 1
        input_7.value = 1
        input_8.value = 1
        input_9.value = 1
        input_10.value = 1
        input_11.value = 1
        input_12.value = 1
        input_13.value = 1
        input_14.value = 1
        input_15.value = 1
        input_16.value = 1
        assert parity_bit.value == 0

        input_1.value = 0
        input_2.value = 1
        input_3.value = 1
        input_4.value = 1
        input_5.value = 1
        input_6.value = 1
        input_7.value = 1
        input_8.value = 1
        input_9.value = 1
        input_10.value = 1
        input_11.value = 1
        input_12.value = 1
        input_13.value = 1
        input_14.value = 1
        input_15.value = 1
        input_16.value = 0
        assert parity_bit.value == 0

        input_1.value = 1
        input_2.value = 1
        input_3.value = 1
        input_4.value = 1
        input_5.value = 1
        input_6.value = 1
        input_7.value = 1
        input_8.value = 1
        input_9.value = 1
        input_10.value = 1
        input_11.value = 1
        input_12.value = 1
        input_13.value = 1
        input_14.value = 1
        input_15.value = 0
        input_16.value = 0
        assert parity_bit.value == 0

        input_1.value = 0
        input_2.value = 1
        input_3.value = 1
        input_4.value = 1
        input_5.value = 1
        input_6.value = 1
        input_7.value = 1
        input_8.value = 1
        input_9.value = 1
        input_10.value = 1
        input_11.value = 1
        input_12.value = 1
        input_13.value = 1
        input_14.value = 1
        input_15.value = 1
        input_16.value = 1
        assert parity_bit.value == 1

        input_1.value = 1
        input_2.value = 1
        input_3.value = 1
        input_4.value = 1
        input_5.value = 1
        input_6.value = 1
        input_7.value = 1
        input_8.value = 1
        input_9.value = 1
        input_10.value = 1
        input_11.value = 1
        input_12.value = 1
        input_13.value = 1
        input_14.value = 1
        input_15.value = 1
        input_16.value = 0
        assert parity_bit.value == 1

        input_1.value = 1
        input_2.value = 1
        input_3.value = 1
        input_4.value = 1
        input_5.value = 1
        input_6.value = 1
        input_7.value = 1
        input_8.value = 1
        input_9.value = 1
        input_10.value = 1
        input_11.value = 1
        input_12.value = 1
        input_13.value = 1
        input_14.value = 1
        input_15.value = 1
        input_16.value = 1
        assert parity_bit.value == 0

        print(a.__doc__)
        print(a)

        a(
            input_bus=(0, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1),
            parity_bit=None
        )
        assert parity_bit.value == 1
