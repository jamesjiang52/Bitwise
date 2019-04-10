import bitwise as bw


class TestSevenSegmentConverterQuad():
    def test_SevenSegmentConverterQuad(self):
        enable = bw.wire.Wire()
        input_1 = bw.wire.Wire()
        input_2 = bw.wire.Wire()
        input_3 = bw.wire.Wire()
        input_4 = bw.wire.Wire()
        input_5 = bw.wire.Wire()
        input_6 = bw.wire.Wire()
        input_7 = bw.wire.Wire()
        input_8 = bw.wire.Wire()
        input_9 = bw.wire.Wire()
        input_10 = bw.wire.Wire()
        input_11 = bw.wire.Wire()
        input_12 = bw.wire.Wire()
        input_13 = bw.wire.Wire()
        input_14 = bw.wire.Wire()
        input_15 = bw.wire.Wire()
        input_16 = bw.wire.Wire()
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
        output_15 = bw.wire.Wire()
        output_16 = bw.wire.Wire()
        output_17 = bw.wire.Wire()
        output_18 = bw.wire.Wire()
        output_19 = bw.wire.Wire()
        output_20 = bw.wire.Wire()
        output_21 = bw.wire.Wire()
        output_22 = bw.wire.Wire()
        output_23 = bw.wire.Wire()
        output_24 = bw.wire.Wire()
        output_25 = bw.wire.Wire()
        output_26 = bw.wire.Wire()
        output_27 = bw.wire.Wire()
        output_28 = bw.wire.Wire()
        input_bus = bw.wire.Bus16(
            input_1,
            input_2,
            input_3,
            input_4,
            input_5,
            input_6,
            input_7,
            input_8,
            input_9,
            input_10,
            input_11,
            input_12,
            input_13,
            input_14,
            input_15,
            input_16
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
        output_bus_3 = bw.wire.BusSevenSegmentDisplay(
            output_15,
            output_16,
            output_17,
            output_18,
            output_19,
            output_20,
            output_21
        )
        output_bus_4 = bw.wire.BusSevenSegmentDisplay(
            output_22,
            output_23,
            output_24,
            output_25,
            output_26,
            output_27,
            output_28
        )

        a = bw.signal.SevenSegmentConverterQuad(
            enable,
            input_bus,
            output_bus_1,
            output_bus_2,
            output_bus_3,
            output_bus_4
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
        input_9.value = 0
        input_10.value = 0
        input_11.value = 0
        input_12.value = 0
        input_13.value = 0
        input_14.value = 0
        input_15.value = 0
        input_16.value = 0
        assert output_bus_1.wire_values == (1, 1, 1, 1, 1, 1, 1)
        assert output_bus_2.wire_values == (1, 1, 1, 1, 1, 1, 1)
        assert output_bus_3.wire_values == (1, 1, 1, 1, 1, 1, 1)
        assert output_bus_4.wire_values == (1, 1, 1, 1, 1, 1, 1)

        enable.value = 0
        input_1.value = 1
        input_2.value = 1
        input_3.value = 1
        input_4.value = 1
        input_5.value = 1
        input_6.value = 1
        input_7.value = 1
        input_8.value = 1
        input_9.value = 1
        input_10.value = 1
        input_11.value = 1
        input_12.value = 1
        input_13.value = 1
        input_14.value = 1
        input_15.value = 1
        input_16.value = 1
        assert output_bus_1.wire_values == (1, 1, 1, 1, 1, 1, 1)
        assert output_bus_2.wire_values == (1, 1, 1, 1, 1, 1, 1)
        assert output_bus_3.wire_values == (1, 1, 1, 1, 1, 1, 1)
        assert output_bus_4.wire_values == (1, 1, 1, 1, 1, 1, 1)

        enable.value = 1
        input_1.value = 0
        input_2.value = 0
        input_3.value = 0
        input_4.value = 0
        input_5.value = 0
        input_6.value = 0
        input_7.value = 0
        input_8.value = 0
        input_9.value = 0
        input_10.value = 0
        input_11.value = 0
        input_12.value = 0
        input_13.value = 0
        input_14.value = 0
        input_15.value = 0
        input_16.value = 0
        assert output_bus_1.wire_values == (1, 0, 0, 0, 0, 0, 0)
        assert output_bus_2.wire_values == (1, 0, 0, 0, 0, 0, 0)
        assert output_bus_3.wire_values == (1, 0, 0, 0, 0, 0, 0)
        assert output_bus_4.wire_values == (1, 0, 0, 0, 0, 0, 0)

        enable.value = 1
        input_1.value = 0
        input_2.value = 0
        input_3.value = 0
        input_4.value = 0
        input_5.value = 0
        input_6.value = 0
        input_7.value = 0
        input_8.value = 0
        input_9.value = 0
        input_10.value = 0
        input_11.value = 0
        input_12.value = 0
        input_13.value = 1
        input_14.value = 1
        input_15.value = 1
        input_16.value = 1
        assert output_bus_1.wire_values == (1, 0, 0, 0, 0, 0, 0)
        assert output_bus_2.wire_values == (1, 0, 0, 0, 0, 0, 0)
        assert output_bus_3.wire_values == (1, 0, 0, 0, 0, 0, 0)
        assert output_bus_4.wire_values == (0, 0, 0, 1, 1, 1, 0)

        enable.value = 1
        input_1.value = 0
        input_2.value = 0
        input_3.value = 0
        input_4.value = 0
        input_5.value = 0
        input_6.value = 0
        input_7.value = 0
        input_8.value = 0
        input_9.value = 1
        input_10.value = 1
        input_11.value = 1
        input_12.value = 1
        input_13.value = 0
        input_14.value = 0
        input_15.value = 0
        input_16.value = 0
        assert output_bus_1.wire_values == (1, 0, 0, 0, 0, 0, 0)
        assert output_bus_2.wire_values == (1, 0, 0, 0, 0, 0, 0)
        assert output_bus_3.wire_values == (0, 0, 0, 1, 1, 1, 0)
        assert output_bus_4.wire_values == (1, 0, 0, 0, 0, 0, 0)

        enable.value = 1
        input_1.value = 0
        input_2.value = 0
        input_3.value = 0
        input_4.value = 0
        input_5.value = 1
        input_6.value = 1
        input_7.value = 1
        input_8.value = 1
        input_9.value = 0
        input_10.value = 0
        input_11.value = 0
        input_12.value = 0
        input_13.value = 0
        input_14.value = 0
        input_15.value = 0
        input_16.value = 0
        assert output_bus_1.wire_values == (1, 0, 0, 0, 0, 0, 0)
        assert output_bus_2.wire_values == (0, 0, 0, 1, 1, 1, 0)
        assert output_bus_3.wire_values == (1, 0, 0, 0, 0, 0, 0)
        assert output_bus_4.wire_values == (1, 0, 0, 0, 0, 0, 0)

        enable.value = 1
        input_1.value = 1
        input_2.value = 1
        input_3.value = 1
        input_4.value = 1
        input_5.value = 0
        input_6.value = 0
        input_7.value = 0
        input_8.value = 0
        input_9.value = 0
        input_10.value = 0
        input_11.value = 0
        input_12.value = 0
        input_13.value = 0
        input_14.value = 0
        input_15.value = 0
        input_16.value = 0
        assert output_bus_1.wire_values == (0, 0, 0, 1, 1, 1, 0)
        assert output_bus_2.wire_values == (1, 0, 0, 0, 0, 0, 0)
        assert output_bus_3.wire_values == (1, 0, 0, 0, 0, 0, 0)
        assert output_bus_4.wire_values == (1, 0, 0, 0, 0, 0, 0)

        enable.value = 1
        input_1.value = 1
        input_2.value = 1
        input_3.value = 1
        input_4.value = 1
        input_5.value = 1
        input_6.value = 1
        input_7.value = 1
        input_8.value = 1
        input_9.value = 1
        input_10.value = 1
        input_11.value = 1
        input_12.value = 1
        input_13.value = 1
        input_14.value = 1
        input_15.value = 1
        input_16.value = 1
        assert output_bus_1.wire_values == (0, 0, 0, 1, 1, 1, 0)
        assert output_bus_2.wire_values == (0, 0, 0, 1, 1, 1, 0)
        assert output_bus_3.wire_values == (0, 0, 0, 1, 1, 1, 0)
        assert output_bus_4.wire_values == (0, 0, 0, 1, 1, 1, 0)

        print(a.__doc__)
        print(a)
