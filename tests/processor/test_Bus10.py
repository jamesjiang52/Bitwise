import bitwise as bw


class TestBus10:
    def test_Bus10(self):
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

        bus_1 = bw.processor.Bus10(
            wire_1,
            wire_2,
            wire_3,
            wire_4,
            wire_5,
            wire_6,
            wire_7,
            wire_8,
            wire_9,
            wire_10
        )

        assert bus_1.wire_values == (
            0, 0, 0, 0, 0, 0, 0, 0, 0, 0)

        wire_1.value = 1
        assert bus_1.wire_values == (
            1, 0, 0, 0, 0, 0, 0, 0, 0, 0)

        wire_2.value = 1
        assert bus_1.wire_values == (
            1, 1, 0, 0, 0, 0, 0, 0, 0, 0)

        wire_3.value = 1
        assert bus_1.wire_values == (
            1, 1, 1, 0, 0, 0, 0, 0, 0, 0)

        wire_4.value = 1
        assert bus_1.wire_values == (
            1, 1, 1, 1, 0, 0, 0, 0, 0, 0)

        wire_5.value = 1
        assert bus_1.wire_values == (
            1, 1, 1, 1, 1, 0, 0, 0, 0, 0)

        wire_6.value = 1
        assert bus_1.wire_values == (
            1, 1, 1, 1, 1, 1, 0, 0, 0, 0)

        wire_7.value = 1
        assert bus_1.wire_values == (
            1, 1, 1, 1, 1, 1, 1, 0, 0, 0)

        wire_8.value = 1
        assert bus_1.wire_values == (
            1, 1, 1, 1, 1, 1, 1, 1, 0, 0)

        assert bus_1[0].value == 1
        assert bus_1[1].value == 1
        assert bus_1[2].value == 1
        assert bus_1[3].value == 1
        assert bus_1[4].value == 1
        assert bus_1[5].value == 1
        assert bus_1[6].value == 1
        assert bus_1[7].value == 1
        assert bus_1[8].value == 0
        assert bus_1[9].value == 0
        assert [i.value for i in bus_1[0:8]] == [1, 1, 1, 1, 1, 1, 1, 1]
        assert [i.value for i in bus_1[8:10]] == [0, 0]
        assert [i.value for i in bus_1[0:10]] == [
            1, 1, 1, 1, 1, 1, 1, 1, 0, 0]

        wire_9.value = 1
        assert bus_1.wire_values == (
            1, 1, 1, 1, 1, 1, 1, 1, 1, 0)

        wire_10.value = 1
        assert bus_1.wire_values == (
            1, 1, 1, 1, 1, 1, 1, 1, 1, 1)

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
        assert bus_1.wire_values == (
            0, 0, 0, 0, 0, 0, 0, 0, 0, 0)

        assert len(bus_1) == 10

        bus_2 = bw.processor.Bus10()
        assert bus_2.wire_values == (
            0, 0, 0, 0, 0, 0, 0, 0, 0, 0)

        bus_2[0].value = 1
        bus_2[2].value = 1
        bus_2[4].value = 1
        bus_2[6].value = 1
        bus_2[8].value = 1
        assert bus_2.wire_values == (
            1, 0, 1, 0, 1, 0, 1, 0, 1, 0)

        bus_1.wire_values = (0, 1, 1, 0, 0, 1, 1, 0, 0, 1)
        assert bus_1.wire_values == (
            0, 1, 1, 0, 0, 1, 1, 0, 0, 1)

        bus_2.wire_values = (1, 0, 0, 1, 1, 0, 0, 1, 1, 0)
        assert bus_2.wire_values == (
            1, 0, 0, 1, 1, 0, 0, 1, 1, 0)
