import bitwise as bw


class TestMultiplier8:
    def test_Multiplier8(self):
        a_1 = bw.wire.Wire()
        a_2 = bw.wire.Wire()
        a_3 = bw.wire.Wire()
        a_4 = bw.wire.Wire()
        a_5 = bw.wire.Wire()
        a_6 = bw.wire.Wire()
        a_7 = bw.wire.Wire()
        a_8 = bw.wire.Wire()
        b_1 = bw.wire.Wire()
        b_2 = bw.wire.Wire()
        b_3 = bw.wire.Wire()
        b_4 = bw.wire.Wire()
        b_5 = bw.wire.Wire()
        b_6 = bw.wire.Wire()
        b_7 = bw.wire.Wire()
        b_8 = bw.wire.Wire()
        product_1 = bw.wire.Wire()
        product_2 = bw.wire.Wire()
        product_3 = bw.wire.Wire()
        product_4 = bw.wire.Wire()
        product_5 = bw.wire.Wire()
        product_6 = bw.wire.Wire()
        product_7 = bw.wire.Wire()
        product_8 = bw.wire.Wire()
        product_9 = bw.wire.Wire()
        product_10 = bw.wire.Wire()
        product_11 = bw.wire.Wire()
        product_12 = bw.wire.Wire()
        product_13 = bw.wire.Wire()
        product_14 = bw.wire.Wire()
        product_15 = bw.wire.Wire()
        product_16 = bw.wire.Wire()
        a_bus = bw.wire.Bus8(a_1, a_2, a_3, a_4, a_5, a_6, a_7, a_8)
        b_bus = bw.wire.Bus8(b_1, b_2, b_3, b_4, b_5, b_6, b_7, b_8)
        product_bus = bw.wire.Bus16(
            product_1,
            product_2,
            product_3,
            product_4,
            product_5,
            product_6,
            product_7,
            product_8,
            product_9,
            product_10,
            product_11,
            product_12,
            product_13,
            product_14,
            product_15,
            product_16
        )

        a = bw.arithmetic.Multiplier8(a_bus, b_bus, product_bus)

        a_1.value = 0
        a_2.value = 0
        a_3.value = 0
        a_4.value = 0
        a_5.value = 0
        a_6.value = 0
        a_7.value = 0
        a_8.value = 0
        b_1.value = 0
        b_2.value = 0
        b_3.value = 0
        b_4.value = 0
        b_5.value = 0
        b_6.value = 0
        b_7.value = 0
        b_8.value = 0
        assert product_bus.wire_values == (
            0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0)

        a_1.value = 0
        a_2.value = 0
        a_3.value = 0
        a_4.value = 0
        a_5.value = 0
        a_6.value = 0
        a_7.value = 0
        a_8.value = 0
        b_1.value = 0
        b_2.value = 0
        b_3.value = 0
        b_4.value = 0
        b_5.value = 0
        b_6.value = 0
        b_7.value = 0
        b_8.value = 1
        assert product_bus.wire_values == (
            0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0)

        a_1.value = 0
        a_2.value = 0
        a_3.value = 0
        a_4.value = 0
        a_5.value = 0
        a_6.value = 0
        a_7.value = 0
        a_8.value = 0
        b_1.value = 0
        b_2.value = 0
        b_3.value = 0
        b_4.value = 0
        b_5.value = 0
        b_6.value = 0
        b_7.value = 1
        b_8.value = 0
        assert product_bus.wire_values == (
            0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0)

        a_1.value = 0
        a_2.value = 0
        a_3.value = 0
        a_4.value = 0
        a_5.value = 0
        a_6.value = 0
        a_7.value = 0
        a_8.value = 0
        b_1.value = 0
        b_2.value = 0
        b_3.value = 0
        b_4.value = 0
        b_5.value = 0
        b_6.value = 1
        b_7.value = 0
        b_8.value = 0
        assert product_bus.wire_values == (
            0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0)

        a_1.value = 0
        a_2.value = 0
        a_3.value = 0
        a_4.value = 0
        a_5.value = 0
        a_6.value = 0
        a_7.value = 0
        a_8.value = 0
        b_1.value = 0
        b_2.value = 0
        b_3.value = 0
        b_4.value = 0
        b_5.value = 1
        b_6.value = 0
        b_7.value = 0
        b_8.value = 0
        assert product_bus.wire_values == (
            0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0)

        a_1.value = 0
        a_2.value = 0
        a_3.value = 0
        a_4.value = 0
        a_5.value = 0
        a_6.value = 0
        a_7.value = 0
        a_8.value = 0
        b_1.value = 0
        b_2.value = 0
        b_3.value = 0
        b_4.value = 1
        b_5.value = 0
        b_6.value = 0
        b_7.value = 0
        b_8.value = 0
        assert product_bus.wire_values == (
            0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0)

        a_1.value = 0
        a_2.value = 0
        a_3.value = 0
        a_4.value = 0
        a_5.value = 0
        a_6.value = 0
        a_7.value = 0
        a_8.value = 0
        b_1.value = 0
        b_2.value = 0
        b_3.value = 1
        b_4.value = 0
        b_5.value = 0
        b_6.value = 0
        b_7.value = 0
        b_8.value = 0
        assert product_bus.wire_values == (
            0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0)

        a_1.value = 0
        a_2.value = 0
        a_3.value = 0
        a_4.value = 0
        a_5.value = 0
        a_6.value = 0
        a_7.value = 0
        a_8.value = 0
        b_1.value = 0
        b_2.value = 1
        b_3.value = 0
        b_4.value = 0
        b_5.value = 0
        b_6.value = 0
        b_7.value = 0
        b_8.value = 0
        assert product_bus.wire_values == (
            0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0)

        a_1.value = 0
        a_2.value = 0
        a_3.value = 0
        a_4.value = 0
        a_5.value = 0
        a_6.value = 0
        a_7.value = 0
        a_8.value = 0
        b_1.value = 1
        b_2.value = 0
        b_3.value = 0
        b_4.value = 0
        b_5.value = 0
        b_6.value = 0
        b_7.value = 0
        b_8.value = 0
        assert product_bus.wire_values == (
            0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0)

        a_1.value = 0
        a_2.value = 0
        a_3.value = 0
        a_4.value = 0
        a_5.value = 0
        a_6.value = 0
        a_7.value = 0
        a_8.value = 1
        b_1.value = 0
        b_2.value = 0
        b_3.value = 0
        b_4.value = 0
        b_5.value = 0
        b_6.value = 0
        b_7.value = 0
        b_8.value = 0
        assert product_bus.wire_values == (
            0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0)

        a_1.value = 0
        a_2.value = 0
        a_3.value = 0
        a_4.value = 0
        a_5.value = 0
        a_6.value = 0
        a_7.value = 1
        a_8.value = 0
        b_1.value = 0
        b_2.value = 0
        b_3.value = 0
        b_4.value = 0
        b_5.value = 0
        b_6.value = 0
        b_7.value = 0
        b_8.value = 0
        assert product_bus.wire_values == (
            0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0)

        a_1.value = 0
        a_2.value = 0
        a_3.value = 0
        a_4.value = 0
        a_5.value = 0
        a_6.value = 1
        a_7.value = 0
        a_8.value = 0
        b_1.value = 0
        b_2.value = 0
        b_3.value = 0
        b_4.value = 0
        b_5.value = 0
        b_6.value = 0
        b_7.value = 0
        b_8.value = 0
        assert product_bus.wire_values == (
            0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0)

        a_1.value = 0
        a_2.value = 0
        a_3.value = 0
        a_4.value = 0
        a_5.value = 1
        a_6.value = 0
        a_7.value = 0
        a_8.value = 0
        b_1.value = 0
        b_2.value = 0
        b_3.value = 0
        b_4.value = 0
        b_5.value = 0
        b_6.value = 0
        b_7.value = 0
        b_8.value = 0
        assert product_bus.wire_values == (
            0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0)

        a_1.value = 0
        a_2.value = 0
        a_3.value = 0
        a_4.value = 1
        a_5.value = 0
        a_6.value = 0
        a_7.value = 0
        a_8.value = 0
        b_1.value = 0
        b_2.value = 0
        b_3.value = 0
        b_4.value = 0
        b_5.value = 0
        b_6.value = 0
        b_7.value = 0
        b_8.value = 0
        assert product_bus.wire_values == (
            0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0)

        a_1.value = 0
        a_2.value = 0
        a_3.value = 1
        a_4.value = 0
        a_5.value = 0
        a_6.value = 0
        a_7.value = 0
        a_8.value = 0
        b_1.value = 0
        b_2.value = 0
        b_3.value = 0
        b_4.value = 0
        b_5.value = 0
        b_6.value = 0
        b_7.value = 0
        b_8.value = 0
        assert product_bus.wire_values == (
            0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0)

        a_1.value = 0
        a_2.value = 1
        a_3.value = 0
        a_4.value = 0
        a_5.value = 0
        a_6.value = 0
        a_7.value = 0
        a_8.value = 0
        b_1.value = 0
        b_2.value = 0
        b_3.value = 0
        b_4.value = 0
        b_5.value = 0
        b_6.value = 0
        b_7.value = 0
        b_8.value = 0
        assert product_bus.wire_values == (
            0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0)

        a_1.value = 1
        a_2.value = 0
        a_3.value = 0
        a_4.value = 0
        a_5.value = 0
        a_6.value = 0
        a_7.value = 0
        a_8.value = 0
        b_1.value = 0
        b_2.value = 0
        b_3.value = 0
        b_4.value = 0
        b_5.value = 0
        b_6.value = 0
        b_7.value = 0
        b_8.value = 0
        assert product_bus.wire_values == (
            0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0)

        a_1.value = 1
        a_2.value = 1
        a_3.value = 1
        a_4.value = 1
        a_5.value = 1
        a_6.value = 1
        a_7.value = 1
        a_8.value = 1
        b_1.value = 0
        b_2.value = 0
        b_3.value = 0
        b_4.value = 0
        b_5.value = 0
        b_6.value = 0
        b_7.value = 0
        b_8.value = 0
        assert product_bus.wire_values == (
            0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0)

        a_1.value = 1
        a_2.value = 1
        a_3.value = 1
        a_4.value = 1
        a_5.value = 1
        a_6.value = 1
        a_7.value = 1
        a_8.value = 1
        b_1.value = 0
        b_2.value = 0
        b_3.value = 0
        b_4.value = 0
        b_5.value = 0
        b_6.value = 0
        b_7.value = 0
        b_8.value = 1
        assert product_bus.wire_values == (
            0, 0, 0, 0, 0, 0, 0, 0, 1, 1, 1, 1, 1, 1, 1, 1)

        a_1.value = 1
        a_2.value = 1
        a_3.value = 1
        a_4.value = 1
        a_5.value = 1
        a_6.value = 1
        a_7.value = 1
        a_8.value = 1
        b_1.value = 0
        b_2.value = 0
        b_3.value = 0
        b_4.value = 0
        b_5.value = 0
        b_6.value = 0
        b_7.value = 1
        b_8.value = 0
        assert product_bus.wire_values == (
            0, 0, 0, 0, 0, 0, 0, 1, 1, 1, 1, 1, 1, 1, 1, 0)

        a_1.value = 1
        a_2.value = 1
        a_3.value = 1
        a_4.value = 1
        a_5.value = 1
        a_6.value = 1
        a_7.value = 1
        a_8.value = 1
        b_1.value = 0
        b_2.value = 0
        b_3.value = 0
        b_4.value = 0
        b_5.value = 0
        b_6.value = 1
        b_7.value = 0
        b_8.value = 0
        assert product_bus.wire_values == (
            0, 0, 0, 0, 0, 0, 1, 1, 1, 1, 1, 1, 1, 1, 0, 0)

        a_1.value = 1
        a_2.value = 1
        a_3.value = 1
        a_4.value = 1
        a_5.value = 1
        a_6.value = 1
        a_7.value = 1
        a_8.value = 1
        b_1.value = 0
        b_2.value = 0
        b_3.value = 0
        b_4.value = 0
        b_5.value = 1
        b_6.value = 0
        b_7.value = 0
        b_8.value = 0
        assert product_bus.wire_values == (
            0, 0, 0, 0, 0, 1, 1, 1, 1, 1, 1, 1, 1, 0, 0, 0)

        a_1.value = 1
        a_2.value = 1
        a_3.value = 1
        a_4.value = 1
        a_5.value = 1
        a_6.value = 1
        a_7.value = 1
        a_8.value = 1
        b_1.value = 0
        b_2.value = 0
        b_3.value = 0
        b_4.value = 1
        b_5.value = 0
        b_6.value = 0
        b_7.value = 0
        b_8.value = 0
        assert product_bus.wire_values == (
            0, 0, 0, 0, 1, 1, 1, 1, 1, 1, 1, 1, 0, 0, 0, 0)

        a_1.value = 1
        a_2.value = 1
        a_3.value = 1
        a_4.value = 1
        a_5.value = 1
        a_6.value = 1
        a_7.value = 1
        a_8.value = 1
        b_1.value = 0
        b_2.value = 0
        b_3.value = 1
        b_4.value = 0
        b_5.value = 0
        b_6.value = 0
        b_7.value = 0
        b_8.value = 0
        assert product_bus.wire_values == (
            0, 0, 0, 1, 1, 1, 1, 1, 1, 1, 1, 0, 0, 0, 0, 0)

        a_1.value = 1
        a_2.value = 1
        a_3.value = 1
        a_4.value = 1
        a_5.value = 1
        a_6.value = 1
        a_7.value = 1
        a_8.value = 1
        b_1.value = 0
        b_2.value = 1
        b_3.value = 0
        b_4.value = 0
        b_5.value = 0
        b_6.value = 0
        b_7.value = 0
        b_8.value = 0
        assert product_bus.wire_values == (
            0, 0, 1, 1, 1, 1, 1, 1, 1, 1, 0, 0, 0, 0, 0, 0)

        a_1.value = 1
        a_2.value = 1
        a_3.value = 1
        a_4.value = 1
        a_5.value = 1
        a_6.value = 1
        a_7.value = 1
        a_8.value = 1
        b_1.value = 1
        b_2.value = 0
        b_3.value = 0
        b_4.value = 0
        b_5.value = 0
        b_6.value = 0
        b_7.value = 0
        b_8.value = 0
        assert product_bus.wire_values == (
            0, 1, 1, 1, 1, 1, 1, 1, 1, 0, 0, 0, 0, 0, 0, 0)

        a_1.value = 0
        a_2.value = 0
        a_3.value = 0
        a_4.value = 0
        a_5.value = 0
        a_6.value = 0
        a_7.value = 0
        a_8.value = 0
        b_1.value = 1
        b_2.value = 1
        b_3.value = 1
        b_4.value = 1
        b_5.value = 1
        b_6.value = 1
        b_7.value = 1
        b_8.value = 1
        assert product_bus.wire_values == (
            0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0)

        a_1.value = 0
        a_2.value = 0
        a_3.value = 0
        a_4.value = 0
        a_5.value = 0
        a_6.value = 0
        a_7.value = 0
        a_8.value = 1
        b_1.value = 1
        b_2.value = 1
        b_3.value = 1
        b_4.value = 1
        b_5.value = 1
        b_6.value = 1
        b_7.value = 1
        b_8.value = 1
        assert product_bus.wire_values == (
            0, 0, 0, 0, 0, 0, 0, 0, 1, 1, 1, 1, 1, 1, 1, 1)

        a_1.value = 0
        a_2.value = 0
        a_3.value = 0
        a_4.value = 0
        a_5.value = 0
        a_6.value = 0
        a_7.value = 1
        a_8.value = 0
        b_1.value = 1
        b_2.value = 1
        b_3.value = 1
        b_4.value = 1
        b_5.value = 1
        b_6.value = 1
        b_7.value = 1
        b_8.value = 1
        assert product_bus.wire_values == (
            0, 0, 0, 0, 0, 0, 0, 1, 1, 1, 1, 1, 1, 1, 1, 0)

        a_1.value = 0
        a_2.value = 0
        a_3.value = 0
        a_4.value = 0
        a_5.value = 0
        a_6.value = 1
        a_7.value = 0
        a_8.value = 0
        b_1.value = 1
        b_2.value = 1
        b_3.value = 1
        b_4.value = 1
        b_5.value = 1
        b_6.value = 1
        b_7.value = 1
        b_8.value = 1
        assert product_bus.wire_values == (
            0, 0, 0, 0, 0, 0, 1, 1, 1, 1, 1, 1, 1, 1, 0, 0)

        a_1.value = 0
        a_2.value = 0
        a_3.value = 0
        a_4.value = 0
        a_5.value = 1
        a_6.value = 0
        a_7.value = 0
        a_8.value = 0
        b_1.value = 1
        b_2.value = 1
        b_3.value = 1
        b_4.value = 1
        b_5.value = 1
        b_6.value = 1
        b_7.value = 1
        b_8.value = 1
        assert product_bus.wire_values == (
            0, 0, 0, 0, 0, 1, 1, 1, 1, 1, 1, 1, 1, 0, 0, 0)

        a_1.value = 0
        a_2.value = 0
        a_3.value = 0
        a_4.value = 1
        a_5.value = 0
        a_6.value = 0
        a_7.value = 0
        a_8.value = 0
        b_1.value = 1
        b_2.value = 1
        b_3.value = 1
        b_4.value = 1
        b_5.value = 1
        b_6.value = 1
        b_7.value = 1
        b_8.value = 1
        assert product_bus.wire_values == (
            0, 0, 0, 0, 1, 1, 1, 1, 1, 1, 1, 1, 0, 0, 0, 0)

        a_1.value = 0
        a_2.value = 0
        a_3.value = 1
        a_4.value = 0
        a_5.value = 0
        a_6.value = 0
        a_7.value = 0
        a_8.value = 0
        b_1.value = 1
        b_2.value = 1
        b_3.value = 1
        b_4.value = 1
        b_5.value = 1
        b_6.value = 1
        b_7.value = 1
        b_8.value = 1
        assert product_bus.wire_values == (
            0, 0, 0, 1, 1, 1, 1, 1, 1, 1, 1, 0, 0, 0, 0, 0)

        a_1.value = 0
        a_2.value = 1
        a_3.value = 0
        a_4.value = 0
        a_5.value = 0
        a_6.value = 0
        a_7.value = 0
        a_8.value = 0
        b_1.value = 1
        b_2.value = 1
        b_3.value = 1
        b_4.value = 1
        b_5.value = 1
        b_6.value = 1
        b_7.value = 1
        b_8.value = 1
        assert product_bus.wire_values == (
            0, 0, 1, 1, 1, 1, 1, 1, 1, 1, 0, 0, 0, 0, 0, 0)

        a_1.value = 1
        a_2.value = 0
        a_3.value = 0
        a_4.value = 0
        a_5.value = 0
        a_6.value = 0
        a_7.value = 0
        a_8.value = 0
        b_1.value = 1
        b_2.value = 1
        b_3.value = 1
        b_4.value = 1
        b_5.value = 1
        b_6.value = 1
        b_7.value = 1
        b_8.value = 1
        assert product_bus.wire_values == (
            0, 1, 1, 1, 1, 1, 1, 1, 1, 0, 0, 0, 0, 0, 0, 0)

        a_1.value = 1
        a_2.value = 1
        a_3.value = 1
        a_4.value = 1
        a_5.value = 1
        a_6.value = 1
        a_7.value = 1
        a_8.value = 1
        b_1.value = 1
        b_2.value = 1
        b_3.value = 1
        b_4.value = 1
        b_5.value = 1
        b_6.value = 1
        b_7.value = 1
        b_8.value = 1
        assert product_bus.wire_values == (
            1, 1, 1, 1, 1, 1, 1, 0, 0, 0, 0, 0, 0, 0, 0, 1)

        print(a.__doc__)
        print(a)
