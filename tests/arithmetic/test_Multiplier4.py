import bitwise as bw


class TestMultiplier4:
    def test_Multiplier4(self):
        a_1 = bw.wire.Wire()
        a_2 = bw.wire.Wire()
        a_3 = bw.wire.Wire()
        a_4 = bw.wire.Wire()
        b_1 = bw.wire.Wire()
        b_2 = bw.wire.Wire()
        b_3 = bw.wire.Wire()
        b_4 = bw.wire.Wire()
        product_1 = bw.wire.Wire()
        product_2 = bw.wire.Wire()
        product_3 = bw.wire.Wire()
        product_4 = bw.wire.Wire()
        product_5 = bw.wire.Wire()
        product_6 = bw.wire.Wire()
        product_7 = bw.wire.Wire()
        product_8 = bw.wire.Wire()
        a_bus = bw.wire.Bus4(a_1, a_2, a_3, a_4)
        b_bus = bw.wire.Bus4(b_1, b_2, b_3, b_4)
        product_bus = bw.wire.Bus8(
            product_1,
            product_2,
            product_3,
            product_4,
            product_5,
            product_6,
            product_7,
            product_8
        )

        a = bw.arithmetic.Multiplier4(a_bus, b_bus, product_bus)

        a_1.value = 0
        a_2.value = 0
        a_3.value = 0
        a_4.value = 0
        b_1.value = 0
        b_2.value = 0
        b_3.value = 0
        b_4.value = 0
        assert product_bus.wire_values == (0, 0, 0, 0, 0, 0, 0, 0)

        a_1.value = 0
        a_2.value = 0
        a_3.value = 0
        a_4.value = 0
        b_1.value = 1
        b_2.value = 1
        b_3.value = 1
        b_4.value = 1
        assert product_bus.wire_values == (0, 0, 0, 0, 0, 0, 0, 0)

        a_1.value = 0
        a_2.value = 0
        a_3.value = 0
        a_4.value = 1
        b_1.value = 0
        b_2.value = 0
        b_3.value = 0
        b_4.value = 0
        assert product_bus.wire_values == (0, 0, 0, 0, 0, 0, 0, 0)

        a_1.value = 0
        a_2.value = 0
        a_3.value = 0
        a_4.value = 1
        b_1.value = 1
        b_2.value = 1
        b_3.value = 1
        b_4.value = 1
        assert product_bus.wire_values == (0, 0, 0, 0, 1, 1, 1, 1)

        a_1.value = 0
        a_2.value = 0
        a_3.value = 1
        a_4.value = 0
        b_1.value = 0
        b_2.value = 0
        b_3.value = 0
        b_4.value = 0
        assert product_bus.wire_values == (0, 0, 0, 0, 0, 0, 0, 0)

        a_1.value = 0
        a_2.value = 0
        a_3.value = 1
        a_4.value = 0
        b_1.value = 1
        b_2.value = 1
        b_3.value = 1
        b_4.value = 1
        assert product_bus.wire_values == (0, 0, 0, 1, 1, 1, 1, 0)

        a_1.value = 0
        a_2.value = 1
        a_3.value = 0
        a_4.value = 0
        b_1.value = 0
        b_2.value = 0
        b_3.value = 0
        b_4.value = 0
        assert product_bus.wire_values == (0, 0, 0, 0, 0, 0, 0, 0)

        a_1.value = 0
        a_2.value = 1
        a_3.value = 0
        a_4.value = 0
        b_1.value = 1
        b_2.value = 1
        b_3.value = 1
        b_4.value = 1
        assert product_bus.wire_values == (0, 0, 1, 1, 1, 1, 0, 0)

        a_1.value = 1
        a_2.value = 0
        a_3.value = 0
        a_4.value = 0
        b_1.value = 0
        b_2.value = 0
        b_3.value = 0
        b_4.value = 0
        assert product_bus.wire_values == (0, 0, 0, 0, 0, 0, 0, 0)

        a_1.value = 1
        a_2.value = 0
        a_3.value = 0
        a_4.value = 0
        b_1.value = 1
        b_2.value = 1
        b_3.value = 1
        b_4.value = 1
        assert product_bus.wire_values == (0, 1, 1, 1, 1, 0, 0, 0)

        a_1.value = 1
        a_2.value = 1
        a_3.value = 1
        a_4.value = 1
        b_1.value = 0
        b_2.value = 0
        b_3.value = 0
        b_4.value = 0
        assert product_bus.wire_values == (0, 0, 0, 0, 0, 0, 0, 0)

        a_1.value = 0
        a_2.value = 0
        a_3.value = 0
        a_4.value = 0
        b_1.value = 0
        b_2.value = 0
        b_3.value = 0
        b_4.value = 1
        assert product_bus.wire_values == (0, 0, 0, 0, 0, 0, 0, 0)

        a_1.value = 1
        a_2.value = 1
        a_3.value = 1
        a_4.value = 1
        b_1.value = 0
        b_2.value = 0
        b_3.value = 0
        b_4.value = 1
        assert product_bus.wire_values == (0, 0, 0, 0, 1, 1, 1, 1)

        a_1.value = 0
        a_2.value = 0
        a_3.value = 0
        a_4.value = 0
        b_1.value = 0
        b_2.value = 0
        b_3.value = 1
        b_4.value = 0
        assert product_bus.wire_values == (0, 0, 0, 0, 0, 0, 0, 0)

        a_1.value = 1
        a_2.value = 1
        a_3.value = 1
        a_4.value = 1
        b_1.value = 0
        b_2.value = 0
        b_3.value = 1
        b_4.value = 0
        assert product_bus.wire_values == (0, 0, 0, 1, 1, 1, 1, 0)

        a_1.value = 0
        a_2.value = 0
        a_3.value = 0
        a_4.value = 0
        b_1.value = 0
        b_2.value = 1
        b_3.value = 0
        b_4.value = 0
        assert product_bus.wire_values == (0, 0, 0, 0, 0, 0, 0, 0)

        a_1.value = 1
        a_2.value = 1
        a_3.value = 1
        a_4.value = 1
        b_1.value = 0
        b_2.value = 1
        b_3.value = 0
        b_4.value = 0
        assert product_bus.wire_values == (0, 0, 1, 1, 1, 1, 0, 0)

        a_1.value = 0
        a_2.value = 0
        a_3.value = 0
        a_4.value = 0
        b_1.value = 1
        b_2.value = 0
        b_3.value = 0
        b_4.value = 0
        assert product_bus.wire_values == (0, 0, 0, 0, 0, 0, 0, 0)

        a_1.value = 1
        a_2.value = 1
        a_3.value = 1
        a_4.value = 1
        b_1.value = 1
        b_2.value = 0
        b_3.value = 0
        b_4.value = 0
        assert product_bus.wire_values == (0, 1, 1, 1, 1, 0, 0, 0)

        a_1.value = 1
        a_2.value = 1
        a_3.value = 1
        a_4.value = 1
        b_1.value = 1
        b_2.value = 1
        b_3.value = 1
        b_4.value = 1
        assert product_bus.wire_values == (1, 1, 1, 0, 0, 0, 0, 1)

        print(a.__doc__)
        print(a)

        a(
            a_bus=(0, 0, 0, 0),
            b_bus=(0, 0, 0, 0),
            product_bus=None
        )
        assert product_bus.wire_values == (0, 0, 0, 0, 0, 0, 0, 0)
