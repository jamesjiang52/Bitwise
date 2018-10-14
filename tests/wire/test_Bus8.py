import bitwise as bw


class TestBus8:
    def test_Bus8(self):
        wire_1 = bw.wire.Wire()
        wire_2 = bw.wire.Wire()
        wire_3 = bw.wire.Wire()
        wire_4 = bw.wire.Wire()
        wire_5 = bw.wire.Wire()
        wire_6 = bw.wire.Wire()
        wire_7 = bw.wire.Wire()
        wire_8 = bw.wire.Wire()

        bus_1 = bw.wire.Bus8(
            wire_1,
            wire_2,
            wire_3,
            wire_4,
            wire_5,
            wire_6,
            wire_7,
            wire_8
        )

        assert bus_1.wire_values == (0, 0, 0, 0, 0, 0, 0, 0)

        wire_1.value = 1
        assert bus_1.wire_values == (1, 0, 0, 0, 0, 0, 0, 0)

        wire_2.value = 1
        assert bus_1.wire_values == (1, 1, 0, 0, 0, 0, 0, 0)

        wire_3.value = 1
        assert bus_1.wire_values == (1, 1, 1, 0, 0, 0, 0, 0)

        wire_4.value = 1
        assert bus_1.wire_values == (1, 1, 1, 1, 0, 0, 0, 0)

        wire_5.value = 1
        assert bus_1.wire_values == (1, 1, 1, 1, 1, 0, 0, 0)

        wire_6.value = 1
        assert bus_1.wire_values == (1, 1, 1, 1, 1, 1, 0, 0)

        wire_7.value = 1
        assert bus_1.wire_values == (1, 1, 1, 1, 1, 1, 1, 0)

        wire_8.value = 1
        assert bus_1.wire_values == (1, 1, 1, 1, 1, 1, 1, 1)

        wire_1.value = 0
        wire_2.value = 0
        wire_3.value = 0
        wire_4.value = 0
        wire_5.value = 0
        wire_6.value = 0
        wire_7.value = 0
        wire_8.value = 0
        assert bus_1.wire_values == (0, 0, 0, 0, 0, 0, 0, 0)