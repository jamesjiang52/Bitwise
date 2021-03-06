import bitwise as bw


class TestComparator15:
    def test_Comparator15(self):
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
        input_17 = bw.wire.Wire()
        input_18 = bw.wire.Wire()
        input_19 = bw.wire.Wire()
        input_20 = bw.wire.Wire()
        input_21 = bw.wire.Wire()
        input_22 = bw.wire.Wire()
        input_23 = bw.wire.Wire()
        input_24 = bw.wire.Wire()
        input_25 = bw.wire.Wire()
        input_26 = bw.wire.Wire()
        input_27 = bw.wire.Wire()
        input_28 = bw.wire.Wire()
        input_29 = bw.wire.Wire()
        input_30 = bw.wire.Wire()
        input_31 = bw.wire.Wire()
        input_32 = bw.wire.Wire()
        gt = bw.wire.Wire()
        z = bw.wire.Wire()
        lt = bw.wire.Wire()
        input_bus_1 = bw.wire.Bus16(
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
        input_bus_2 = bw.wire.Bus16(
            input_17,
            input_18,
            input_19,
            input_20,
            input_21,
            input_22,
            input_23,
            input_24,
            input_25,
            input_26,
            input_27,
            input_28,
            input_29,
            input_30,
            input_31,
            input_32
        )

        a = bw.logic.Comparator15(input_bus_1, input_bus_2, gt, z, lt)

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
        input_17.value = 0
        input_18.value = 0
        input_19.value = 0
        input_20.value = 0
        input_21.value = 0
        input_22.value = 0
        input_23.value = 0
        input_24.value = 0
        input_25.value = 0
        input_26.value = 0
        input_27.value = 0
        input_28.value = 0
        input_29.value = 0
        input_30.value = 0
        input_31.value = 0
        input_32.value = 0
        assert (gt.value, z.value, lt.value) == (0, 1, 0)

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
        input_17.value = 0
        input_18.value = 0
        input_19.value = 0
        input_20.value = 0
        input_21.value = 0
        input_22.value = 0
        input_23.value = 0
        input_24.value = 0
        input_25.value = 0
        input_26.value = 0
        input_27.value = 0
        input_28.value = 0
        input_29.value = 0
        input_30.value = 0
        input_31.value = 0
        input_32.value = 1
        assert (gt.value, z.value, lt.value) == (0, 1, 0)

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
        input_17.value = 1
        input_18.value = 0
        input_19.value = 0
        input_20.value = 0
        input_21.value = 0
        input_22.value = 0
        input_23.value = 0
        input_24.value = 0
        input_25.value = 0
        input_26.value = 0
        input_27.value = 0
        input_28.value = 0
        input_29.value = 0
        input_30.value = 0
        input_31.value = 0
        input_32.value = 0
        assert (gt.value, z.value, lt.value) == (0, 1, 0)

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
        input_17.value = 1
        input_18.value = 1
        input_19.value = 1
        input_20.value = 1
        input_21.value = 1
        input_22.value = 1
        input_23.value = 1
        input_24.value = 1
        input_25.value = 1
        input_26.value = 1
        input_27.value = 1
        input_28.value = 1
        input_29.value = 1
        input_30.value = 1
        input_31.value = 1
        input_32.value = 1
        assert (gt.value, z.value, lt.value) == (0, 1, 0)

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
        input_17.value = 0
        input_18.value = 0
        input_19.value = 0
        input_20.value = 0
        input_21.value = 0
        input_22.value = 0
        input_23.value = 0
        input_24.value = 0
        input_25.value = 0
        input_26.value = 0
        input_27.value = 0
        input_28.value = 0
        input_29.value = 0
        input_30.value = 0
        input_31.value = 0
        input_32.value = 0
        assert (gt.value, z.value, lt.value) == (1, 0, 0)

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
        input_17.value = 1
        input_18.value = 1
        input_19.value = 1
        input_20.value = 1
        input_21.value = 1
        input_22.value = 1
        input_23.value = 1
        input_24.value = 1
        input_25.value = 1
        input_26.value = 1
        input_27.value = 1
        input_28.value = 1
        input_29.value = 1
        input_30.value = 1
        input_31.value = 1
        input_32.value = 1
        assert (gt.value, z.value, lt.value) == (1, 0, 0)

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
        input_17.value = 0
        input_18.value = 1
        input_19.value = 1
        input_20.value = 1
        input_21.value = 1
        input_22.value = 1
        input_23.value = 1
        input_24.value = 1
        input_25.value = 1
        input_26.value = 1
        input_27.value = 1
        input_28.value = 1
        input_29.value = 1
        input_30.value = 1
        input_31.value = 1
        input_32.value = 0
        assert (gt.value, z.value, lt.value) == (1, 0, 0)

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
        input_17.value = 1
        input_18.value = 1
        input_19.value = 1
        input_20.value = 1
        input_21.value = 1
        input_22.value = 1
        input_23.value = 1
        input_24.value = 1
        input_25.value = 1
        input_26.value = 1
        input_27.value = 1
        input_28.value = 1
        input_29.value = 1
        input_30.value = 1
        input_31.value = 1
        input_32.value = 0
        assert (gt.value, z.value, lt.value) == (1, 0, 0)

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
        input_17.value = 0
        input_18.value = 0
        input_19.value = 0
        input_20.value = 0
        input_21.value = 0
        input_22.value = 0
        input_23.value = 0
        input_24.value = 0
        input_25.value = 0
        input_26.value = 0
        input_27.value = 0
        input_28.value = 0
        input_29.value = 0
        input_30.value = 0
        input_31.value = 0
        input_32.value = 1
        assert (gt.value, z.value, lt.value) == (0, 0, 1)

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
        input_17.value = 0
        input_18.value = 0
        input_19.value = 0
        input_20.value = 0
        input_21.value = 0
        input_22.value = 0
        input_23.value = 0
        input_24.value = 0
        input_25.value = 0
        input_26.value = 0
        input_27.value = 0
        input_28.value = 0
        input_29.value = 0
        input_30.value = 0
        input_31.value = 0
        input_32.value = 0
        assert (gt.value, z.value, lt.value) == (0, 0, 1)

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
        input_17.value = 0
        input_18.value = 1
        input_19.value = 1
        input_20.value = 1
        input_21.value = 1
        input_22.value = 1
        input_23.value = 1
        input_24.value = 1
        input_25.value = 1
        input_26.value = 1
        input_27.value = 1
        input_28.value = 1
        input_29.value = 1
        input_30.value = 1
        input_31.value = 1
        input_32.value = 1
        assert (gt.value, z.value, lt.value) == (0, 0, 1)

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
        input_17.value = 1
        input_18.value = 1
        input_19.value = 1
        input_20.value = 1
        input_21.value = 1
        input_22.value = 1
        input_23.value = 1
        input_24.value = 1
        input_25.value = 1
        input_26.value = 1
        input_27.value = 1
        input_28.value = 1
        input_29.value = 1
        input_30.value = 1
        input_31.value = 1
        input_32.value = 1
        assert (gt.value, z.value, lt.value) == (0, 0, 1)

        print(a.__doc__)
        print(a)

        a(
            a_bus=(0, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1),
            b_bus=(0, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 0),
            greater_than=None,
            equal_to=None,
            less_than=None
        )
        assert (gt.value, z.value, lt.value) == (1, 0, 0)
