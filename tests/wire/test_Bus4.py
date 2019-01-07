import bitwise as bw


class TestBus4:
    def test_Bus4(self):
        wire_1 = bw.wire.Wire()
        wire_2 = bw.wire.Wire()
        wire_3 = bw.wire.Wire()
        wire_4 = bw.wire.Wire()

        bus_1 = bw.wire.Bus4(
            wire_1,
            wire_2,
            wire_3,
            wire_4
        )

        assert bus_1.wire_values == (0, 0, 0, 0)

        wire_1.value = 1
        assert bus_1.wire_values == (1, 0, 0, 0)

        wire_2.value = 1
        assert bus_1.wire_values == (1, 1, 0, 0)

        assert bus_1[0].value == 1
        assert bus_1[1].value == 1
        assert bus_1[2].value == 0
        assert bus_1[3].value == 0
        assert [i.value for i in bus_1[0:2]] == [1, 1]
        assert [i.value for i in bus_1[2:4]] == [0, 0]
        assert [i.value for i in bus_1[0:4]] == [1, 1, 0, 0]

        wire_3.value = 1
        assert bus_1.wire_values == (1, 1, 1, 0)

        wire_4.value = 1
        assert bus_1.wire_values == (1, 1, 1, 1)

        wire_1.value = 0
        wire_2.value = 0
        wire_3.value = 0
        wire_4.value = 0
        assert bus_1.wire_values == (0, 0, 0, 0)

        assert len(bus_1) == 4

        bus_2 = bw.wire.Bus4()
        assert bus_2.wire_values == (0, 0, 0, 0)

        bus_2[0].value = 1
        bus_2[2].value = 1
        assert bus_2.wire_values == (1, 0, 1, 0)

        bus_1.wire_values = (0, 1, 1, 0)
        assert bus_1.wire_values == (0, 1, 1, 0)

        bus_2.wire_values = (1, 0, 0, 1)
        assert bus_2.wire_values == (1, 0, 0, 1)
