import bitwise as bw


class TestBus16:
    def test_Bus16(self):
        wire_1 = bw.wire.Wire()
        wire_2 = bw.wire.Wire()
        wire_3 = bw.wire.Wire()
        wire_4 = bw.wire.Wire()
        wire_5 = bw.wire.Wire()
        wire_6 = bw.wire.Wire()
        wire_7 = bw.wire.Wire()
        wire_8 = bw.wire.Wire()
        wire_9 = bw.wire.Wire()
        wire_10 = bw.wire.Wire()
        wire_11 = bw.wire.Wire()
        wire_12 = bw.wire.Wire()
        wire_13 = bw.wire.Wire()
        wire_14 = bw.wire.Wire()
        wire_15 = bw.wire.Wire()
        wire_16 = bw.wire.Wire()

        bus_1 = bw.wire.Bus16(
            wire_1,
            wire_2,
            wire_3,
            wire_4,
            wire_5,
            wire_6,
            wire_7,
            wire_8,
            wire_9,
            wire_10,
            wire_11,
            wire_12,
            wire_13,
            wire_14,
            wire_15,
            wire_16
        )

        assert bus_1.wire_values == (
            0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0)

        wire_1.value = 1
        assert bus_1.wire_values == (
            1, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0)

        wire_2.value = 1
        assert bus_1.wire_values == (
            1, 1, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0)

        wire_3.value = 1
        assert bus_1.wire_values == (
            1, 1, 1, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0)

        wire_4.value = 1
        assert bus_1.wire_values == (
            1, 1, 1, 1, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0)

        wire_5.value = 1
        assert bus_1.wire_values == (
            1, 1, 1, 1, 1, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0)

        wire_6.value = 1
        assert bus_1.wire_values == (
            1, 1, 1, 1, 1, 1, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0)

        wire_7.value = 1
        assert bus_1.wire_values == (
            1, 1, 1, 1, 1, 1, 1, 0, 0, 0, 0, 0, 0, 0, 0, 0)

        wire_8.value = 1
        assert bus_1.wire_values == (
            1, 1, 1, 1, 1, 1, 1, 1, 0, 0, 0, 0, 0, 0, 0, 0)

        wire_9.value = 1
        assert bus_1.wire_values == (
            1, 1, 1, 1, 1, 1, 1, 1, 1, 0, 0, 0, 0, 0, 0, 0)

        wire_10.value = 1
        assert bus_1.wire_values == (
            1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 0, 0, 0, 0, 0, 0)

        wire_11.value = 1
        assert bus_1.wire_values == (
            1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 0, 0, 0, 0, 0)

        wire_12.value = 1
        assert bus_1.wire_values == (
            1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 0, 0, 0, 0)

        wire_13.value = 1
        assert bus_1.wire_values == (
            1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 0, 0, 0)

        wire_14.value = 1
        assert bus_1.wire_values == (
            1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 0, 0)

        wire_15.value = 1
        assert bus_1.wire_values == (
            1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 0)

        wire_16.value = 1
        assert bus_1.wire_values == (
            1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1)

        wire_1.value = 0
        wire_2.value = 0
        wire_3.value = 0
        wire_4.value = 0
        wire_5.value = 0
        wire_6.value = 0
        wire_7.value = 0
        wire_8.value = 0
        wire_9.value = 0
        wire_10.value = 0
        wire_11.value = 0
        wire_12.value = 0
        wire_13.value = 0
        wire_14.value = 0
        wire_15.value = 0
        wire_16.value = 0
        assert bus_1.wire_values == (
            0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0)
