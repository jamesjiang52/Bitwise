"""
The following classes are defined:
    Multiplexer2To1
    Multiplexer4To1
    Multiplexer8To1
    Multiplexer16To1
"""

from .. import wire
from .. import gate

Wire = wire.Wire
Bus4 = wire.Bus4
Bus8 = wire.Bus8
Bus16 = wire.Bus16


class Multiplexer2To1:
    """Construct a new 2-to-1 multiplexer.

    Args:
        enable: An object of type Wire. Enables the multiplexer.
        select: An object of type Wire. The select input.
        input_1: An object of type Wire. The first data input to the
            multiplexer.
        input_2: An object of type Wire. The second data input to the
            multiplexer.
        output: An object of type Wire. The output of the multiplexer. Takes on
            the value of input_1 for a 1 select and input_2 for a 0 select.
    """
    def __init__(self, enable, select, input_1, input_2, output):
        wire_1 = Wire()
        wire_2 = Wire()
        wire_3 = Wire()
        wire_4 = Wire()

        gate.NOTGate(select, wire_1)
        gate.ANDGate2(select, input_1, wire_2)
        gate.ANDGate2(wire_1, input_2, wire_3)
        gate.ORGate2(wire_2, wire_3, wire_4)
        gate.ANDGate2(enable, wire_4, output)

        self.enable = enable
        self.select = select
        self.input_1 = input_1
        self.input_2 = input_2
        self.output = output

    def __str__(self):
        str_ = ""
        str_ += "enable: " + str(self.enable.value) + "\n"
        str_ += "select: " + str(self.select.value) + "\n"
        str_ += "input_1: " + str(self.input_1.value) + "\n"
        str_ += "input_2: " + str(self.input_2.value) + "\n"
        str_ += "output: " + str(self.output.value)
        return str_


class Multiplexer4To1:
    """Construct a new 4-to-1 multiplexer.

    Args:
        enable: An object of type Wire. Enables the multiplexer.
        select_1: An object of type Wire. The most significant bit of the
            select input.
        select_2: An object of type Wire. The least significant bit of the
            select input.
        input_bus: An object of type Bus4. The data input to the multiplexer.
        output: An object of type Wire. The output of the multiplexer. Takes on
            the value of input_bus[0] for a (1, 1) select and input_bus[3] for
            a (0, 0) select.

    Raises:
        TypeError: If input_bus is not a bus of width 4.
    """
    def __init__(self, enable, select_1, select_2, input_bus, output):
        if len(input_bus.wires) != 4:
            raise TypeError(
                "Expected bus of width 4, received bus of width {0}.".format(
                    len(input_bus.wires)
                )
            )

        wire_1 = Wire()
        wire_2 = Wire()

        Multiplexer2To1(enable, select_2, *input_bus.wires[0:2], wire_1)
        Multiplexer2To1(enable, select_2, *input_bus.wires[2:4], wire_2)
        Multiplexer2To1(enable, select_1, wire_1, wire_2, output)

        self.enable = enable
        self.select_1 = select_1
        self.select_2 = select_2
        self.input_bus = input_bus
        self.output = output

    def __str__(self):
        str_ = ""
        str_ += "enable: " + str(self.enable.value) + "\n"
        str_ += "select_1: " + str(self.select_1.value) + "\n"
        str_ += "select_2: " + str(self.select_2.value) + "\n"
        str_ += "input_bus: " + self.input_bus.__str__() + "\n"
        str_ += "output: " + str(self.output.value)
        return str_


class Multiplexer8To1:
    """Construct a new 8-to-1 multiplexer.

    Args:
        enable: An object of type Wire. Enables the multiplexer.
        select_1: An object of type Wire. The most significant bit of the
            select input.
        select_2: An object of type Wire.
        select_3: An object of type Wire. The least significant bit of the
            select input.
        input_bus: An object of type Bus8. The data input to the multiplexer.
        output: An object of type Wire. The output of the multiplexer. Takes on
            the value of input_bus[0] for a (1, 1, 1) select and input_bus[7]
            for a (0, 0, 0) select.

    Raises:
        TypeError: If input_bus is not a bus of width 8.
    """
    def __init__(
            self, enable, select_1, select_2, select_3, input_bus, output):
        if len(input_bus.wires) != 8:
            raise TypeError(
                "Expected bus of width 8, received bus of width {0}.".format(
                    len(input_bus.wires)
                )
            )

        wire_1 = Wire()
        wire_2 = Wire()
        bus_1 = Bus4(*input_bus.wires[0:4])
        bus_2 = Bus4(*input_bus.wires[4:8])

        Multiplexer4To1(enable, select_2, select_3, bus_1, wire_1)
        Multiplexer4To1(enable, select_2, select_3, bus_2, wire_2)
        Multiplexer2To1(enable, select_1, wire_1, wire_2, output)

        self.enable = enable
        self.select_1 = select_1
        self.select_2 = select_2
        self.select_3 = select_3
        self.input_bus = input_bus
        self.output = output

    def __str__(self):
        str_ = ""
        str_ += "enable: " + str(self.enable.value) + "\n"
        str_ += "select_1: " + str(self.select_1.value) + "\n"
        str_ += "select_2: " + str(self.select_2.value) + "\n"
        str_ += "select_3: " + str(self.select_3.value) + "\n"
        str_ += "input_bus: " + self.input_bus.__str__() + "\n"
        str_ += "output: " + str(self.output.value)
        return str_


class Multiplexer16To1:
    """Construct a new 16-to-1 multiplexer.

    Args:
        enable: An object of type Wire. Enables the multiplexer.
        select_bus: An object of type Bus4. select_bus[0] and select_bus[3] are
            the most and least significant bit, respectively.
        input_bus: An object of type Bus16. The data input to the multiplexer.
        output: An object of type Wire. The output of the multiplexer. Takes on
            the value of input_bus[0] for a (1, 1, 1, 1) select and
            input_bus[15] for a (0, 0, 0, 0) select.

    Raises:
        TypeError: If select_bus is not a bus of width 4, or if input_bus is
            not a bus of width 16.
    """
    def __init__(self, enable, select_bus, input_bus, output):
        if len(input_bus.wires) != 16:
            raise TypeError(
                "Expected bus of width 16, received bus of width {0}.".format(
                    len(input_bus.wires)
                )
            )

        if len(select_bus.wires) != 4:
            raise TypeError(
                "Expected bus of width 4, received bus of width {0}.".format(
                    len(select_bus.wires)
                )
            )

        wire_1 = Wire()
        wire_2 = Wire()
        wire_3 = Wire()
        wire_4 = Wire()
        bus_1 = Bus4(*input_bus.wires[0:4])
        bus_2 = Bus4(*input_bus.wires[4:8])
        bus_3 = Bus4(*input_bus.wires[8:12])
        bus_4 = Bus4(*input_bus.wires[12:16])
        bus_5 = Bus4(wire_1, wire_2, wire_3, wire_4)

        Multiplexer4To1(enable, *select_bus.wires[2:4], bus_1, wire_1)
        Multiplexer4To1(enable, *select_bus.wires[2:4], bus_2, wire_2)
        Multiplexer4To1(enable, *select_bus.wires[2:4], bus_3, wire_3)
        Multiplexer4To1(enable, *select_bus.wires[2:4], bus_4, wire_4)
        Multiplexer4To1(enable, *select_bus.wires[0:2], bus_5, output)

        self.enable = enable
        self.select_bus = select_bus
        self.input_bus = input_bus
        self.output = output

    def __str__(self):
        str_ = ""
        str_ += "enable: " + str(self.enable.value) + "\n"
        str_ += "select_bus: " + self.select_bus.__str__() + "\n"
        str_ += "input_bus: " + self.input_bus.__str__() + "\n"
        str_ += "output: " + str(self.output.value)
        return str_
