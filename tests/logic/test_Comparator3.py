import bitwise as bw


class TestComparator3:
    def test_Comparator3(self):
        input_1 = bw.wire.Wire()
        input_2 = bw.wire.Wire()
        input_3 = bw.wire.Wire()
        input_4 = bw.wire.Wire()
        input_5 = bw.wire.Wire()
        input_6 = bw.wire.Wire()
        input_7 = bw.wire.Wire()
        input_8 = bw.wire.Wire()
        gt = bw.wire.Wire()
        z = bw.wire.Wire()
        lt = bw.wire.Wire()
        input_bus_1 = bw.wire.Bus4(input_1, input_2, input_3, input_4)
        input_bus_2 = bw.wire.Bus4(input_5, input_6, input_7, input_8)

        a = bw.logic.Comparator3(input_bus_1, input_bus_2, gt, z, lt)

        input_1.value = 0
        input_2.value = 0
        input_3.value = 0
        input_4.value = 0
        input_5.value = 0
        input_6.value = 0
        input_7.value = 0
        input_8.value = 0
        assert (gt.value, z.value, lt.value) == (0, 1, 0)

        input_1.value = 0
        input_2.value = 0
        input_3.value = 0
        input_4.value = 1
        input_5.value = 0
        input_6.value = 0
        input_7.value = 0
        input_8.value = 1
        assert (gt.value, z.value, lt.value) == (0, 1, 0)

        input_1.value = 1
        input_2.value = 0
        input_3.value = 0
        input_4.value = 0
        input_5.value = 1
        input_6.value = 0
        input_7.value = 0
        input_8.value = 0
        assert (gt.value, z.value, lt.value) == (0, 1, 0)

        input_1.value = 1
        input_2.value = 1
        input_3.value = 1
        input_4.value = 1
        input_5.value = 1
        input_6.value = 1
        input_7.value = 1
        input_8.value = 1
        assert (gt.value, z.value, lt.value) == (0, 1, 0)

        input_1.value = 0
        input_2.value = 0
        input_3.value = 0
        input_4.value = 1
        input_5.value = 0
        input_6.value = 0
        input_7.value = 0
        input_8.value = 0
        assert (gt.value, z.value, lt.value) == (1, 0, 0)

        input_1.value = 0
        input_2.value = 0
        input_3.value = 0
        input_4.value = 0
        input_5.value = 1
        input_6.value = 1
        input_7.value = 1
        input_8.value = 1
        assert (gt.value, z.value, lt.value) == (1, 0, 0)

        input_1.value = 0
        input_2.value = 1
        input_3.value = 1
        input_4.value = 1
        input_5.value = 0
        input_6.value = 1
        input_7.value = 1
        input_8.value = 0
        assert (gt.value, z.value, lt.value) == (1, 0, 0)

        input_1.value = 1
        input_2.value = 1
        input_3.value = 1
        input_4.value = 1
        input_5.value = 1
        input_6.value = 1
        input_7.value = 1
        input_8.value = 0
        assert (gt.value, z.value, lt.value) == (1, 0, 0)

        input_1.value = 0
        input_2.value = 0
        input_3.value = 0
        input_4.value = 0
        input_5.value = 0
        input_6.value = 0
        input_7.value = 0
        input_8.value = 1
        assert (gt.value, z.value, lt.value) == (0, 0, 1)

        input_1.value = 1
        input_2.value = 1
        input_3.value = 1
        input_4.value = 1
        input_5.value = 0
        input_6.value = 0
        input_7.value = 0
        input_8.value = 0
        assert (gt.value, z.value, lt.value) == (0, 0, 1)

        input_1.value = 0
        input_2.value = 1
        input_3.value = 1
        input_4.value = 0
        input_5.value = 0
        input_6.value = 1
        input_7.value = 1
        input_8.value = 1
        assert (gt.value, z.value, lt.value) == (0, 0, 1)

        input_1.value = 1
        input_2.value = 1
        input_3.value = 1
        input_4.value = 0
        input_5.value = 1
        input_6.value = 1
        input_7.value = 1
        input_8.value = 1
        assert (gt.value, z.value, lt.value) == (0, 0, 1)

        print(a.__doc__)
        print(a)

        a(
            a_bus=(0, 1, 1, 1),
            b_bus=(0, 1, 1, 0),
            greater_than=None,
            equal_to=None,
            less_than=None
        )
        assert (gt.value, z.value, lt.value) == (1, 0, 0)
