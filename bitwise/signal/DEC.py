"""
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
    """Construct a new 1-of-4 decoder.

    Args:
        enable: An object of type Wire. Enables the decoder.
        input_1: An object of type Wire. The most significant bit of the data
            input.
        input_2: An object of type Wire. The least significant bit of the data
            input.
        output_bus: An object of type Bus4. A one-hot encoded value of the
            input, with output_bus[0] corresponding to a (1, 1) input and
            output_bus[3] corresponding to a (0, 0) input.

    Raises:
        TypeError: If output_bus is not a bus of width 4.
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

        self.enable = enable
        self.input_1 = input_1
        self.input_2 = input_2
        self.output_bus = output_bus

    def __str__(self):
        str_ = ""
        str_ += "enable: " + str(self.enable.value) + "\n"
        str_ += "input_1: " + str(self.input_1.value) + "\n"
        str_ += "input_2: " + str(self.input_2.value) + "\n"
        str_ += "output_bus: " + self.output_bus.__str__()
        return str_


class Decoder1Of8:
    """Construct a new 1-of-8 decoder.

    Args:
        enable: An object of type Wire. Enables the decoder.
        input_1: An object of type Wire. The most significant bit of the data
            input.
        input_2: An object of type Wire.
        input_3: An object of type Wire. The least significant bit of the data
            input.
        output_bus: An object of type Bus8. A one-hot encoded value of the data
            input, with output[0] corresponding to a (1, 1, 1) input and
            output[7] corresponding to a (0, 0, 0) input.

    Raises:
        TypeError: If output_bus is not a bus of width 8.
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

        self.enable = enable
        self.input_1 = input_1
        self.input_2 = input_2
        self.input_3 = input_3
        self.output_bus = output_bus

    def __str__(self):
        str_ = ""
        str_ += "enable: " + str(self.enable.value) + "\n"
        str_ += "input_1: " + str(self.input_1.value) + "\n"
        str_ += "input_2: " + str(self.input_2.value) + "\n"
        str_ += "input_3: " + str(self.input_3.value) + "\n"
        str_ += "output_bus: " + self.output_bus.__str__()
        return str_


class Decoder1Of16:
    """
    Construct a new 1-of-16 decoder.

    Args:
        enable: An object of type Wire. Enables the decoder.
        input_bus: An object of type Bus4. The data input to the decoder.
            input[0] and input[3] are the most and least significant bit,
            respectively.
        output_bus: An object of type Bus16. A one-hot encoded value of the
            input, with output[0] corresponding to a (1, 1, 1, 1) input and
            output[15] corresponding to a (0, 0, 0, 0) input.

    Raises:
        TypeError: If input_bus is not a bus of width 4, or if output_bus is
            not a bus of width 16.
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

        self.enable = enable
        self.input_bus = input_bus
        self.output_bus = output_bus

    def __str__(self):
        str_ = ""
        str_ += "enable: " + str(self.enable.value) + "\n"
        str_ += "input_bus: " + self.input_bus.__str__() + "\n"
        str_ += "output_bus: " + self.output_bus.__str__()
        return str_
