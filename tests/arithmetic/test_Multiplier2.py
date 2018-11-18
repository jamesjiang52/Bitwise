import bitwise as bw


class TestMultiplier2:
    def test_Multiplier2(self):
        a_1 = bw.wire.Wire()
        a_2 = bw.wire.Wire()
        b_1 = bw.wire.Wire()
        b_2 = bw.wire.Wire()
        product_1 = bw.wire.Wire()
        product_2 = bw.wire.Wire()
        product_3 = bw.wire.Wire()
        product_4 = bw.wire.Wire()
        product_bus = bw.wire.Bus4(product_1, product_2, product_3, product_4)

        bw.arithmetic.Multiplier2(a_1, a_2, b_1, b_2, product_bus)

        a_1.value = 0
        a_2.value = 0
        b_1.value = 0
        b_2.value = 0
        assert product_bus.wire_values == (0, 0, 0, 0)

        a_1.value = 0
        a_2.value = 0
        b_1.value = 0
        b_2.value = 1
        assert product_bus.wire_values == (0, 0, 0, 0)

        a_1.value = 0
        a_2.value = 0
        b_1.value = 1
        b_2.value = 0
        assert product_bus.wire_values == (0, 0, 0, 0)

        a_1.value = 0
        a_2.value = 0
        b_1.value = 1
        b_2.value = 1
        assert product_bus.wire_values == (0, 0, 0, 0)

        a_1.value = 0
        a_2.value = 1
        b_1.value = 0
        b_2.value = 0
        assert product_bus.wire_values == (0, 0, 0, 0)

        a_1.value = 0
        a_2.value = 1
        b_1.value = 0
        b_2.value = 1
        assert product_bus.wire_values == (0, 0, 0, 1)

        a_1.value = 0
        a_2.value = 1
        b_1.value = 1
        b_2.value = 0
        assert product_bus.wire_values == (0, 0, 1, 0)

        a_1.value = 0
        a_2.value = 1
        b_1.value = 1
        b_2.value = 1
        assert product_bus.wire_values == (0, 0, 1, 1)

        a_1.value = 1
        a_2.value = 0
        b_1.value = 0
        b_2.value = 0
        assert product_bus.wire_values == (0, 0, 0, 0)

        a_1.value = 1
        a_2.value = 0
        b_1.value = 0
        b_2.value = 1
        assert product_bus.wire_values == (0, 0, 1, 0)

        a_1.value = 1
        a_2.value = 0
        b_1.value = 1
        b_2.value = 0
        assert product_bus.wire_values == (0, 1, 0, 0)

        a_1.value = 1
        a_2.value = 0
        b_1.value = 1
        b_2.value = 1
        assert product_bus.wire_values == (0, 1, 1, 0)

        a_1.value = 1
        a_2.value = 1
        b_1.value = 0
        b_2.value = 0
        assert product_bus.wire_values == (0, 0, 0, 0)

        a_1.value = 1
        a_2.value = 1
        b_1.value = 0
        b_2.value = 1
        assert product_bus.wire_values == (0, 0, 1, 1)

        a_1.value = 1
        a_2.value = 1
        b_1.value = 1
        b_2.value = 0
        assert product_bus.wire_values == (0, 1, 1, 0)

        a_1.value = 1
        a_2.value = 1
        b_1.value = 1
        b_2.value = 1
        assert product_bus.wire_values == (1, 0, 0, 1)
