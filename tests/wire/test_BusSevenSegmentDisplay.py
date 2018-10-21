import bitwise as bw


class TestBusSevenSegmentDisplay:
    def test_BusSevenSegmentDisplay(self):
        wire_1 = bw.wire.Wire()
        wire_2 = bw.wire.Wire()
        wire_3 = bw.wire.Wire()
        wire_4 = bw.wire.Wire()
        wire_5 = bw.wire.Wire()
        wire_6 = bw.wire.Wire()
        wire_7 = bw.wire.Wire()

        bus_1 = bw.wire.BusSevenSegmentDisplay(
            wire_1,
            wire_2,
            wire_3,
            wire_4,
            wire_5,
            wire_6,
            wire_7
        )

        assert bus_1.wire_values == (0, 0, 0, 0, 0, 0, 0)

        wire_1.value = 1
        assert bus_1.wire_values == (1, 0, 0, 0, 0, 0, 0)

        wire_2.value = 1
        assert bus_1.wire_values == (1, 1, 0, 0, 0, 0, 0)

        wire_3.value = 1
        assert bus_1.wire_values == (1, 1, 1, 0, 0, 0, 0)

        wire_4.value = 1
        assert bus_1.wire_values == (1, 1, 1, 1, 0, 0, 0)

        assert bus_1[0].value == 1
        assert bus_1[1].value == 1
        assert bus_1[2].value == 1
        assert bus_1[3].value == 1
        assert bus_1[4].value == 0
        assert bus_1[5].value == 0
        assert bus_1[6].value == 0
        assert [i.value for i in bus_1[0:4]] == [1, 1, 1, 1]
        assert [i.value for i in bus_1[4:7]] == [0, 0, 0]
        assert [i.value for i in bus_1[0:7]] == [1, 1, 1, 1, 0, 0, 0]

        wire_5.value = 1
        assert bus_1.wire_values == (1, 1, 1, 1, 1, 0, 0)

        wire_6.value = 1
        assert bus_1.wire_values == (1, 1, 1, 1, 1, 1, 0)

        wire_7.value = 1
        assert bus_1.wire_values == (1, 1, 1, 1, 1, 1, 1)

        wire_1.value = 0
        wire_2.value = 0
        wire_3.value = 0
        wire_4.value = 0
        wire_5.value = 0
        wire_6.value = 0
        wire_7.value = 0
        assert bus_1.wire_values == (0, 0, 0, 0, 0, 0, 0)

        assert len(bus_1) == 7
