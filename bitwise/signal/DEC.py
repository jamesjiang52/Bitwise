"""
This module defines classes that simulate decoders. A decoder receives an
encoded value, such as a binary value, and transmits a one-hot output
corresponding to the decoded value. The decoders in this module have an
additional enable input; if this input is 0, all the output values are 0,
regardless of the other inputs.

The following classes are defined:
    Decoder1Of4
    Decoder1Of8
    Decoder1Of16
"""

from .. import wire
from .. import gate

Wire = wire.Wire
Bus4 = wire.Bus4
Bus8 = wire.Bus8
Bus16 = wire.Bus16


class Decoder1Of4:
    """
    This decoder has two data inputs, an enable input, and four outputs in a
    single 4-bit bus:
                     ________
         enable ----|        |---- output_1
        input_1 ----|        |---- output_2
        input_2 ----|        |---- output_3
                    |________|---- output_4

    The inputs have input_1 and input_2 as the MSB and LSB, respectively. The
    outputs have output_1 corresponding to the (1, 1) input and output_4
    corresponding to the (0, 0) input. If the enable is 0, the outputs are
    all 0, regardless of input.
    """
    def __init__(self, enable, input_1, input_2, output_bus):
        if len(output_bus.wires) != 4:
            raise TypeError(
                "Expected bus of width 4, received bus of width {0}.".format(
                    len(output_bus.wires)
                )
            )

        wire_1 = Wire()
        wire_2 = Wire()

        gate.NOTGate(input_1, wire_1)
        gate.NOTGate(input_2, wire_2)

        gate.ANDGate3(enable, input_1, input_2, output_bus.wires[0])
        gate.ANDGate3(enable, input_1, wire_2, output_bus.wires[1])
        gate.ANDGate3(enable, wire_1, input_2, output_bus.wires[2])
        gate.ANDGate3(enable, wire_1, wire_2, output_bus.wires[3])


class Decoder1Of8:
    """
    This decoder has three data inputs, an enable input, and eight outputs in a
    single 8-bit bus:
                     ________
         enable ----|        |---- output_1
        input_1 ----|        |---- output_2
        input_2 ----|        |---- output_3
        input_3 ----|        |---- output_4
                    |        |---- output_5
                    |        |---- output_6
                    |        |---- output_7
                    |________|---- output_8

    The inputs have input_1 and input_3 as the MSB and LSB, respectively. The
    outputs have output_1 corresponding to the (1, 1, 1) input and output_8
    corresponding to the (0, 0, 0) input. If the enable is 0, the outputs are
    all 0, regardless of input.
    """
    def __init__(self, enable, input_1, input_2, input_3, output_bus):
        if len(output_bus.wires) != 8:
            raise TypeError(
                "Expected bus of width 8, received bus of width {0}.".format(
                    len(output_bus.wires)
                )
            )

        wire_1 = Wire()
        wire_2 = Wire()
        wire_3 = Wire()
        bus_1 = Bus4(*output_bus.wires[0:4])
        bus_2 = Bus4(*output_bus.wires[4:8])

        gate.NOTGate(input_1, wire_1)
        gate.ANDGate2(enable, input_1, wire_2)
        gate.ANDGate2(enable, wire_1, wire_3)
        Decoder1Of4(wire_2, input_2, input_3, bus_1)
        Decoder1Of4(wire_3, input_2, input_3, bus_2)


class Decoder1Of16:
    """
    This decoder has four data inputs in a single 4-bit bus, an enable input,
    and sixteen outputs in a single 16-bit bus:
                     ________
         enable ----|        |---- output_1
        input_1 ----|        |---- output_2
        input_2 ----|        |---- output_3
        input_3 ----|        |---- output_4
        input_4 ----|        |---- output_5
                    |        |---- output_6
                    |        |---- output_7
                    |        |---- output_8
                    |        |---- output_9
                    |        |---- output_10
                    |        |---- output_11
                    |        |---- output_12
                    |        |---- output_13
                    |        |---- output_14
                    |        |---- output_15
                    |________|---- output_16

    The inputs have input_1 and input_4 as the MSB and LSB, respectively. The
    outputs have output_1 corresponding to the (1, 1, 1, 1) input and
    output_16 corresponding to the (0, 0, 0, 0) input. If the enable is 0, the
    outputs are all 0, regardless of input.
    """
    def __init__(self, enable, input_bus, output_bus):
        if len(input_bus.wires) != 4:
            raise TypeError(
                "Expected bus of width 4, received bus of width {0}.".format(
                    len(input_bus.wires)
                )
            )

        if len(output_bus.wires) != 16:
            raise TypeError(
                "Expected bus of width 16, received bus of width {0}.".format(
                    len(output_bus.wires)
                )
            )

        wire_1 = Wire()
        wire_2 = Wire()
        wire_3 = Wire()
        wire_4 = Wire()
        bus_1 = Bus4(wire_1, wire_2, wire_3, wire_4)
        bus_2 = Bus4(*output_bus.wires[0:4])
        bus_3 = Bus4(*output_bus.wires[4:8])
        bus_4 = Bus4(*output_bus.wires[8:12])
        bus_5 = Bus4(*output_bus.wires[12:16])

        Decoder1Of4(enable, input_bus.wires[0], input_bus.wires[1], bus_1)
        Decoder1Of4(wire_1, input_bus.wires[2], input_bus.wires[3], bus_2)
        Decoder1Of4(wire_2, input_bus.wires[2], input_bus.wires[3], bus_3)
        Decoder1Of4(wire_3, input_bus.wires[2], input_bus.wires[3], bus_4)
        Decoder1Of4(wire_4, input_bus.wires[2], input_bus.wires[3], bus_5)
