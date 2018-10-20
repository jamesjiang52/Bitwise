"""
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
    """Construct a new 4-to-2 priority encoder.

    Args:
        enable: An object of type Wire. Enables the encoder.
        input_bus: An object of type Bus4. The data input to the encoder.
            input_bus[0] corresponds to an input value of 3, and input_bus[3]
            corresponds to an input value of 0.
        valid: An object of type Wire. The valid indicator. Only takes on the
            value of 0 if all the wires in input_bus have value 0.
        output_1: An object of type Wire. The most significant bit of the
            output.
        output_2: An object of type Wire. The least significant bit of the
            output.

    Raises:
        TypeError: If input_bus is not a bus of width 4.
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
    """Construct a new 8-to-3 priority encoder.

    Args:
        enable: An object of type Wire. Enables the encoder.
        input_bus: An object of type Bus8. The data input to the encoder.
            input_bus[0] corresponds to an input value of 7, and input_bus[7]
            corresponds to an input value of 0.
        valid: An object of type Wire. The valid indicator. Only takes on the
            value of 0 if all the wires in input_bus have value 0.
        output_1: An object of type Wire. The most significant bit of the
            output.
        output_2: An object of type Wire.
        output_3: An object of type Wire. The least significant bit of the
            output.

    Raises:
        TypeError: If input_bus is not a bus of width 8.
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
    """Construct a new 16-to-4 priority encoder.

    Args:
        enable: An object of type Wire. Enables the encoder.
        input_bus: An object of type Bus16. The data input to the encoder.
            input_bus[0] corresponds to an input value of 15, and input_bus[15]
            corresponds to an input value of 0.
        valid: An object of type Wire. The valid indicator. Only takes on the
            value of 0 if all the wires in input_bus have value 0.
        output_bus: An object of type Bus4. The output of the encoder.
            output_bus[0] and output_bus[3] are the most and least significant
            bit, respectively.

    Raises:
        TypeError: If input_bus is not a bus of width 16, or if output_bus is
        not a bus of width 4.
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
