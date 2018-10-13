"""
This module defines classes that simulate seven-segment display converters.

The following classes are defined:
    SevenSegmentConverter
    SevenSegmentConverterDual
    SevenSegmentConverterQuad
"""

from .. import wire
from .. import gate

Wire = wire.Wire
Bus4 = wire.Bus4
BusSevenSegmentDisplay = wire.BusSevenSegmentDisplay


class SevenSegmentConverter:
    """
    This converter converts a four-bit input into a seven-segment display with
    a common anode. It has four inputs in a single 4-bit bus, an enable input,
    and seven outputs in a single 7-bit bus:
                     ________
         enable ----|        |---- output_1
        input_1 ----|        |---- output_2
        input_2 ----|        |---- output_3
        input_3 ----|        |---- output_4
        input_4 ----|        |---- output_5
                    |        |---- output_6
                    |________|---- output_7

    The inputs have input_1 and input_4 as the MSB and LSB, respectively.
    Referring to the below diagram, the outputs have output_1 as G, output_2 as
    F, output_3 as E, and so on. Since the LED's have a common anode, they are
    illuminated by an output of value 0.
        ________
       |   A    |
     F |        | B
       |        |
       |________|
       |   G    |
     E |        | C
       |        |
       |________|
           D

    If the enable is 0, the outputs are all 1, regardless of input.
    """
    def __init__(self, enable, input_bus, output_bus):
        if len(input_bus.wires) != 4:
            raise TypeError(
                "Expected bus of width 4, received bus of width {0}.".format(
                    len(input_bus.wires)
                )
            )

        if len(output_bus.wires) != 7:
            raise TypeError(
                "Expected bus of width 7, received bus of width {0}.".format(
                    len(output_bus.wires)
                )
            )

        input_1_not = Wire()
        input_2_not = Wire()
        input_3_not = Wire()
        input_4_not = Wire()
        M_1 = Wire()
        M_2 = Wire()
        M_3 = Wire()
        M_4 = Wire()
        M_5 = Wire()
        M_6 = Wire()
        M_7 = Wire()
        M_8 = Wire()
        M_9 = Wire()
        M_10 = Wire()
        M_11 = Wire()
        M_12 = Wire()
        M_13 = Wire()
        M_14 = Wire()
        M_15 = Wire()
        M_16 = Wire()
        wire_AND_1 = Wire()
        wire_AND_2 = Wire()
        wire_AND_3 = Wire()
        wire_AND_4 = Wire()
        wire_AND_5 = Wire()
        wire_AND_6 = Wire()
        wire_AND_7 = Wire()
        output_1 = Wire()
        output_2 = Wire()
        output_3 = Wire()
        output_4 = Wire()
        output_5 = Wire()
        output_6 = Wire()
        output_7 = Wire()

        wires = input_bus.wires

        gate.NOTGate(wires[0], input_1_not)
        gate.NOTGate(wires[1], input_2_not)
        gate.NOTGate(wires[2], input_3_not)
        gate.NOTGate(wires[3], input_4_not)

        gate.ORGate4(wires[0], wires[1], wires[2], wires[3], M_1)
        gate.ORGate4(wires[0], wires[1], wires[2], input_4_not, M_2)
        gate.ORGate4(wires[0], wires[1], input_3_not, wires[3], M_3)
        gate.ORGate4(wires[0], wires[1], input_3_not, input_4_not, M_4)
        gate.ORGate4(wires[0], input_2_not, wires[2], wires[3], M_5)
        gate.ORGate4(wires[0], input_2_not, wires[2], input_4_not, M_6)
        gate.ORGate4(wires[0], input_2_not, input_3_not, wires[3], M_7)
        gate.ORGate4(wires[0], input_2_not, input_3_not, input_4_not, M_8)
        gate.ORGate4(input_1_not, wires[1], wires[2], wires[3], M_9)
        gate.ORGate4(input_1_not, wires[1], wires[2], input_4_not, M_10)
        gate.ORGate4(input_1_not, wires[1], input_3_not, wires[3], M_11)
        gate.ORGate4(input_1_not, wires[1], input_3_not, input_4_not, M_12)
        gate.ORGate4(input_1_not, input_2_not, wires[2], wires[3], M_13)
        gate.ORGate4(input_1_not, input_2_not, wires[2], input_4_not, M_14)
        gate.ORGate4(input_1_not, input_2_not, input_3_not, wires[3], M_15)
        gate.ORGate4(input_1_not, input_2_not, input_3_not, input_4_not, M_16)

        gate.ANDGate4(M_2, M_5, M_12, M_14, wire_AND_1)
        gate.ANDGate2(wire_AND_1, enable, output_1)
        gate.ANDGate4(M_6, M_7, M_12, M_13, wire_AND_2)
        gate.ANDGate4(wire_AND_2, enable, M_15, M_16, output_2)
        gate.ANDGate4(M_3, M_13, M_15, M_16, wire_AND_3)
        gate.ANDGate2(wire_AND_3, enable, output_3)
        gate.ANDGate4(M_2, M_5, M_8, M_11, wire_AND_4)
        gate.ANDGate3(wire_AND_4, enable, M_16, output_4)
        gate.ANDGate4(M_2, M_4, M_5, M_6, wire_AND_5)
        gate.ANDGate4(wire_AND_5, enable, M_8, M_10, output_5)
        gate.ANDGate4(M_2, M_3, M_4, M_8, wire_AND_6)
        gate.ANDGate3(wire_AND_6, enable, M_14, output_6)
        gate.ANDGate4(M_1, M_2, M_8, M_13, wire_AND_7)
        gate.ANDGate2(wire_AND_7, enable, output_7)

        gate.NOTGate(output_1, output_bus.wires[6])
        gate.NOTGate(output_2, output_bus.wires[5])
        gate.NOTGate(output_3, output_bus.wires[4])
        gate.NOTGate(output_4, output_bus.wires[3])
        gate.NOTGate(output_5, output_bus.wires[2])
        gate.NOTGate(output_6, output_bus.wires[1])
        gate.NOTGate(output_7, output_bus.wires[0])


class SevenSegmentConverterDual:
    """
    This converter simply stacks two seven-segment displays with common anodes.
    It has eight inputs in a single 8-bit bus, an enable input, and fourteen
    outputs in two 7-bit buses:
                     ________
         enable ----|        |---- output_1
        input_1 ----|        |---- output_2
        input_2 ----|        |---- output_3
        input_3 ----|        |---- output_4
        input_4 ----|        |---- output_5
        input_5 ----|        |---- output_6
        input_6 ----|        |---- output_7
        input_7 ----|        |---- output_8
        input_8 ----|        |---- output_9
                    |        |---- output_10
                    |        |---- output_11
                    |        |---- output_12
                    |        |---- output_13
                    |________|---- output_14

    The inputs have input_1 and input_8 as the MSB and LSB, respectively.
    Inputs input_1 to input_4 are converted to outputs output_1 to output_7
    (the first output bus). Inputs input_5 to input_8 are converted to outputs
    output_8 to output_14 (the second output bus). If the enable is 0, the
    outputs are all 1, regardless of input.
    """
    def __init__(self, enable, input_bus, output_bus_1, output_bus_2):
        if len(input_bus.wires) != 8:
            raise TypeError(
                "Expected bus of width 8, received bus of width {0}.".format(
                    len(input_bus.wires)
                )
            )

        if len(output_bus_1.wires) != 7:
            raise TypeError(
                "Expected bus of width 7, received bus of width {0}.".format(
                    len(output_bus_1.wires)
                )
            )

        if len(output_bus_2.wires) != 7:
            raise TypeError(
                "Expected bus of width 7, received bus of width {0}.".format(
                    len(output_bus_2.wires)
                )
            )

        bus_1 = Bus4(*input_bus.wires[0:4])
        bus_2 = Bus4(*input_bus.wires[4:8])

        SevenSegmentConverter(enable, bus_1, output_bus_1)
        SevenSegmentConverter(enable, bus_2, output_bus_2)


class SevenSegmentConverterQuad:
    """
    This converter simply stacks four seven-segment displays with common
    anodes. It has sixteen inputs in a single 16-bit bus, an enable input, and
    twenty-eight outputs in four 7-bit buses:
                      ________
          enable ----|        |---- output_1
         input_1 ----|        |---- output_2
         input_2 ----|        |---- output_3
         input_3 ----|        |---- output_4
         input_4 ----|        |---- output_5
         input_5 ----|        |---- output_6
         input_6 ----|        |---- output_7
         input_7 ----|        |---- output_8
         input_8 ----|        |---- output_9
         input_9 ----|        |---- output_10
        input_10 ----|        |---- output_11
        input_11 ----|        |---- output_12
        input_12 ----|        |---- output_13
        input_13 ----|        |---- output_14
        input_14 ----|        |---- output_15
        input_15 ----|        |---- output_16
        input_16 ----|        |---- output_17
                     |        |---- output_18
                     |        |---- output_19
                     |        |---- output_20
                     |        |---- output_21
                     |        |---- output_22
                     |        |---- output_23
                     |        |---- output_24
                     |        |---- output_25
                     |        |---- output_26
                     |        |---- output_27
                     |________|---- output_28

    The inputs have input_1 and input_16 as the MSB and LSB, respectively.
    Inputs input_1 to input_4 are converted to outputs output_1 to output_7 (
    the first output bus). Inputs input_5 to input_8 are converted to outputs
    output_8 to output_14 (the second output bus). Inputs input_9 to input_12
    are converted to outputs output_15 to output_21 (the third output bus).
    Inputs input_13 to input_16 are converted to outputs output_22 to output_28
    (the fourth output bus). If the enable is 0, the outputs are all 1,
    regardless of input.
    """
    def __init__(
            self,
            enable,
            input_bus,
            output_bus_1,
            output_bus_2,
            output_bus_3,
            output_bus_4
            ):
        if len(input_bus.wires) != 16:
            raise TypeError(
                "Expected bus of width 16, received bus of width {0}.".format(
                    len(input_bus.wires)
                )
            )

        if len(output_bus_1.wires) != 7:
            raise TypeError(
                "Expected bus of width 7, received bus of width {0}.".format(
                    len(output_bus_1.wires)
                )
            )

        if len(output_bus_2.wires) != 7:
            raise TypeError(
                "Expected bus of width 7, received bus of width {0}.".format(
                    len(output_bus_2.wires)
                )
            )

        if len(output_bus_3.wires) != 7:
            raise TypeError(
                "Expected bus of width 7, received bus of width {0}.".format(
                    len(output_bus_3.wires)
                )
            )

        if len(output_bus_4.wires) != 7:
            raise TypeError(
                "Expected bus of width 7, received bus of width {0}.".format(
                    len(output_bus_4.wires)
                )
            )

        bus_1 = Bus4(*input_bus.wires[0:4])
        bus_2 = Bus4(*input_bus.wires[4:8])
        bus_3 = Bus4(*input_bus.wires[8:12])
        bus_4 = Bus4(*input_bus.wires[12:16])

        SevenSegmentConverter(enable, bus_1, output_bus_1)
        SevenSegmentConverter(enable, bus_2, output_bus_2)
        SevenSegmentConverter(enable, bus_3, output_bus_3)
        SevenSegmentConverter(enable, bus_4, output_bus_4)
