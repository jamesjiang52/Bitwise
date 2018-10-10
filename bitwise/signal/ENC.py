"""
This module defines classes that simulate encoders. An encoder receives a
one-hot input and encodes it into something like a binary value, transmitting
the output. The encoders in this module have an additional enable input; if
this input is 0, all the output values are 0, regardless of the other inputs.
Also, the encoders have an additional output that is 0 if all of the inputs are
0. Otherwise, this output has a value of 1. In the case that more than one of
the inputs is 1, the input representing the largest value takes priority (e.g.
if two inputs are 1 and represent the values 2 and 13, the output will be the
encoded value of 13).

The following classes are defined:
    Encoder4To2
    Encoder8To3
    Encoder16To4
"""

from .. import wire
from .. import gate
from . import MUX

Wire = wire.Wire
Bus4 = wire.Bus4
Bus8 = wire.Bus8
Bus16 = wire.Bus16


class Encoder4To2:
    """
    This priority encoder has four inputs in a single 4-bit bus, an enable
    input, two outputs, and an additional valid output:
                     ________
         enable ----|        |---- valid
        input_1 ----|        |---- output_1
        input_2 ----|        |---- output_2
        input_3 ----|        |
        input_4 ----|________|

    The outputs have output_1 and output_2 as the MSB and LSB, respectively.
    The output takes on the value of (1, 1) for a one-hot input_1 input and
    (0, 0) for a one-hot input_4 input. If the enable is 0, the outputs are all
    0, regardless of input.
    """
    def __init__(self, enable, input_bus, valid, output_1, output_2):
        if len(input_bus.wires) != 4:
            raise TypeError(
                "Expected bus of width 4, received bus of width {0}.".format(
                    len(input_bus.wires)
                )
            )

        wire_1 = Wire()
        wire_2 = Wire()
        wire_3 = Wire()
        wire_4 = Wire()
        wire_5 = Wire()

        gate.NOTGate(input_bus.wires[1], wire_1)
        gate.ANDGate2(wire_1, input_bus.wires[2], wire_2)
        gate.ORGate2(input_bus.wires[0], input_bus.wires[1], wire_3)
        gate.ORGate2(wire_2, input_bus.wires[0], wire_4)
        gate.ORGate4(*input_bus.wires, wire_5)
        gate.ANDGate2(enable, wire_5, valid)
        gate.ANDGate2(enable, wire_3, output_1)
        gate.ANDGate2(enable, wire_4, output_2)


class Encoder8To3:
    """
    This priority encoder has eight inputs in a single 8-bit bus, an enable
    input, three outputs, and an additional valid output:
                     ________
         enable ----|        |---- valid
        input_1 ----|        |---- output_1
        input_2 ----|        |---- output_2
        input_3 ----|        |---- output_3
        input_4 ----|        |
        input_5 ----|        |
        input_6 ----|        |
        input_7 ----|        |
        input_8 ----|________|

    The outputs have output_1 and output_3 as the MSB and LSB, respectively.
    The output takes on the value of (1, 1, 1) for a one-hot input_1 input and
    (0, 0, 0) for a one-hot input_8 input. If the enable is 0, the outputs are
    all 0, regardless of input.
    """
    def __init__(self, enable, input_bus, valid, output_1, output_2, output_3):
        if len(input_bus.wires) != 8:
            raise TypeError(
                "Expected bus of width 8, received bus of width {0}.".format(
                    len(input_bus.wires)
                )
            )

        enc_1_output_1 = Wire()
        enc_1_output_2 = Wire()
        enc_2_valid = Wire()
        enc_2_output_1 = Wire()
        enc_2_output_2 = Wire()
        bus_1 = Bus4(*input_bus.wires[0:4])
        bus_2 = Bus4(*input_bus.wires[4:8])

        Encoder4To2(enable, bus_1, output_1, enc_1_output_1, enc_1_output_2)
        Encoder4To2(enable, bus_2, enc_2_valid, enc_2_output_1, enc_2_output_2)
        MUX.Multiplexer2To1(
            enable,
            output_1,
            enc_1_output_1,
            enc_2_output_1,
            output_2
        )
        MUX.Multiplexer2To1(
            enable,
            output_1,
            enc_1_output_2,
            enc_2_output_2,
            output_3
        )
        gate.ORGate2(output_1, enc_2_valid, valid)


class Encoder16To4:
    """
    This priority encoder has sixteen inputs in a single 16-bit bus, an enable
    input, four outputs in a single 4-bit bus, and an additional valid output:
                      ________
          enable ----|        |---- valid
         input_1 ----|        |---- output_1
         input_2 ----|        |---- output_2
         input_3 ----|        |---- output_3
         input_4 ----|        |---- output_4
         input_5 ----|        |
         input_6 ----|        |
         input_7 ----|        |
         input_8 ----|        |
         input_9 ----|        |
        input_10 ----|        |
        input_11 ----|        |
        input_12 ----|        |
        input_13 ----|        |
        input_14 ----|        |
        input_15 ----|        |
        input_16 ----|________|

    The outputs have output_1 and output_4 as the MSB and LSB, respectively.
    The output takes on the value of (1, 1, 1, 1) for a one-hot input_1 input
    and (0, 0, 0, 0) for a one-hot input_16 input. If the enable is 0, the
    outputs are all 0, regardless of input.
    """
    def __init__(self, enable, input_bus, valid, output_bus):
        if len(input_bus.wires) != 16:
            raise TypeError(
                "Expected bus of width 16, received bus of width {0}.".format(
                    len(input_bus.wires)
                )
            )

        if len(output_bus.wires) != 4:
            raise TypeError(
                "Expected bus of width 4, received bus of width {0}.".format(
                    len(input_bus.wires)
                )
            )

        enc_1_output_1 = Wire()
        enc_1_output_2 = Wire()
        enc_1_output_3 = Wire()
        enc_2_valid = Wire()
        enc_2_output_1 = Wire()
        enc_2_output_2 = Wire()
        enc_2_output_3 = Wire()
        bus_1 = Bus8(*input_bus.wires[0:8])
        bus_2 = Bus8(*input_bus.wires[8:16])
        output = output_bus.wires

        Encoder8To3(
            enable,
            bus_1,
            output[0],
            enc_1_output_1,
            enc_1_output_2,
            enc_1_output_3
        )
        Encoder8To3(
            enable,
            bus_2,
            enc_2_valid,
            enc_2_output_1,
            enc_2_output_2,
            enc_2_output_3
        )
        MUX.Multiplexer2To1(
            enable,
            output[0],
            enc_1_output_1,
            enc_2_output_1,
            output[1]
        )
        MUX.Multiplexer2To1(
            enable,
            output[0],
            enc_1_output_2,
            enc_2_output_2,
            output[2]
        )
        MUX.Multiplexer2To1(
            enable,
            output[0],
            enc_1_output_3,
            enc_2_output_3,
            output[3]
        )
        gate.ORGate2(output[0], enc_2_valid, valid)
