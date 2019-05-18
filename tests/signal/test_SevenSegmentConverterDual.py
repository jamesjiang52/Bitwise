import bitwise as bw


class TestSevenSegmentConverterDual():
    def test_SevenSegmentConverterDual(self):
        enable = bw.wire.Wire()
        input_1 = bw.wire.Wire()
        input_2 = bw.wire.Wire()
        input_3 = bw.wire.Wire()
        input_4 = bw.wire.Wire()
        input_5 = bw.wire.Wire()
        input_6 = bw.wire.Wire()
        input_7 = bw.wire.Wire()
        input_8 = bw.wire.Wire()
        output_1 = bw.wire.Wire()
        output_2 = bw.wire.Wire()
        output_3 = bw.wire.Wire()
        output_4 = bw.wire.Wire()
        output_5 = bw.wire.Wire()
        output_6 = bw.wire.Wire()
        output_7 = bw.wire.Wire()
        output_8 = bw.wire.Wire()
        output_9 = bw.wire.Wire()
        output_10 = bw.wire.Wire()
        output_11 = bw.wire.Wire()
        output_12 = bw.wire.Wire()
        output_13 = bw.wire.Wire()
        output_14 = bw.wire.Wire()
        input_bus = bw.wire.Bus8(
            input_1,
            input_2,
            input_3,
            input_4,
            input_5,
            input_6,
            input_7,
            input_8
        )
        output_bus_1 = bw.wire.BusSevenSegmentDisplay(
            output_1,
            output_2,
            output_3,
            output_4,
            output_5,
            output_6,
            output_7
        )
        output_bus_2 = bw.wire.BusSevenSegmentDisplay(
            output_8,
            output_9,
            output_10,
            output_11,
            output_12,
            output_13,
            output_14
        )

        a = bw.signal.SevenSegmentConverterDual(
            enable,
            input_bus,
            output_bus_1,
            output_bus_2
        )

        enable.value = 0
        input_1.value = 0
        input_2.value = 0
        input_3.value = 0
        input_4.value = 0
        input_5.value = 0
        input_6.value = 0
        input_7.value = 0
        input_8.value = 0
        assert output_bus_1.wire_values == (1, 1, 1, 1, 1, 1, 1)
        assert output_bus_2.wire_values == (1, 1, 1, 1, 1, 1, 1)

        enable.value = 0
        input_1.value = 1
        input_2.value = 1
        input_3.value = 1
        input_4.value = 1
        input_5.value = 1
        input_6.value = 1
        input_7.value = 1
        input_8.value = 1
        assert output_bus_1.wire_values == (1, 1, 1, 1, 1, 1, 1)
        assert output_bus_2.wire_values == (1, 1, 1, 1, 1, 1, 1)

        enable.value = 1
        input_1.value = 0
        input_2.value = 0
        input_3.value = 0
        input_4.value = 0
        input_5.value = 0
        input_6.value = 0
        input_7.value = 0
        input_8.value = 0
        assert output_bus_1.wire_values == (1, 0, 0, 0, 0, 0, 0)
        assert output_bus_2.wire_values == (1, 0, 0, 0, 0, 0, 0)

        enable.value = 1
        input_1.value = 0
        input_2.value = 0
        input_3.value = 0
        input_4.value = 0
        input_5.value = 1
        input_6.value = 1
        input_7.value = 1
        input_8.value = 1
        assert output_bus_1.wire_values == (1, 0, 0, 0, 0, 0, 0)
        assert output_bus_2.wire_values == (0, 0, 0, 1, 1, 1, 0)

        enable.value = 1
        input_1.value = 1
        input_2.value = 1
        input_3.value = 1
        input_4.value = 1
        input_5.value = 0
        input_6.value = 0
        input_7.value = 0
        input_8.value = 0
        assert output_bus_1.wire_values == (0, 0, 0, 1, 1, 1, 0)
        assert output_bus_2.wire_values == (1, 0, 0, 0, 0, 0, 0)

        enable.value = 1
        input_1.value = 1
        input_2.value = 1
        input_3.value = 1
        input_4.value = 1
        input_5.value = 1
        input_6.value = 1
        input_7.value = 1
        input_8.value = 1
        assert output_bus_1.wire_values == (0, 0, 0, 1, 1, 1, 0)
        assert output_bus_2.wire_values == (0, 0, 0, 1, 1, 1, 0)

        print(a.__doc__)
        print(a)

        a(
            enable=1,
            input_bus=(0, 0, 0, 0, 1, 1, 1, 1),
            output_bus_1=None,
            output_bus_2=None
        )
        assert output_bus_1.wire_values == (1, 0, 0, 0, 0, 0, 0)
        assert output_bus_2.wire_values == (0, 0, 0, 1, 1, 1, 0)
