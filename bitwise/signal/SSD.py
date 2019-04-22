"""
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
    """Construct a new seven-segment converter.

    Args:
        enable: An object of type Wire. Enables the seven-segment converter.
        input_bus: An object of type Bus4. The data input to the seven-segment
            converter. input_bus[0] and input_bus[3] are the most and least
            significant bit, respectively.
        output_bus: An object of type BusSevenSegmentDisplay. The output of the
            seven-segment converter. output_bus[0] and output_bus[7] correspond
            to segment G and segment A, respectively.

    Raises:
        TypeError: If input_bus is not a bus of width 4, or if output_bus is
            not a bus of width 7.
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

        self.enable = enable
        self.input_bus = input_bus
        self.output_bus = output_bus

    def __str__(self):
        str_ = ""
        str_ += "enable: " + str(self.enable.value) + "\n"
        str_ += "input_bus: " + self.input_bus.__str__() + "\n"
        str_ += "output_bus: " + self.output_bus.__str__()
        return str_


class SevenSegmentConverterDual:
    """Construct a new dual seven-segment converter.

    Args:
        enable: An object of type Wire. Enables the seven-segment converter.
        input_bus: An object of type Bus8. The data input to the seven-segment
            converter. input_bus[0] and input_bus[7] are the most and least
            significant bit, respectively.
        output_bus_1: An object of type BusSevenSegmentDisplay. The first
            output bus of the seven-segment converter, using input_bus[0] and
            input_bus[3] as the most and least significant bit, respectively.
            output_bus_1[0] and output_bus_1[7] correspond to segment G and
            segment A, respectively.
        output_bus_2: An object of type BusSevenSegmentDisplay. The second
            output bus of the seven-segment converter, using input_bus[4] and
            input_bus[7] as the most and least significant bit, respectively.
            output_bus_2[0] and output_bus_2[7] correspond to segment G and
            segment A, respectively.

    Raises:
        TypeError: If input_bus is not a bus of width 8, or if either
            output_bus_1 or output_bus_2 is not a bus of width 7.
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

        self.enable = enable
        self.input_bus = input_bus
        self.output_bus_1 = output_bus_1
        self.output_bus_2 = output_bus_2

    def __str__(self):
        str_ = ""
        str_ += "enable: " + str(self.enable.value) + "\n"
        str_ += "input_bus: " + self.input_bus.__str__() + "\n"
        str_ += "output_bus_1: " + self.output_bus_1.__str__() + "\n"
        str_ += "output_bus_2: " + self.output_bus_2.__str__()
        return str_


class SevenSegmentConverterQuad:
    """Construct a new quad seven-segment converter.

    Args:
        enable: An object of type Wire. Enables the seven-segment converter.
        input_bus: An object of type Bus16. The data input to the seven-segment
            converter. input_bus[0] and input_bus[15] are the most and least
            significant bit, respectively.
        output_bus_1: An object of type BusSevenSegmentDisplay. The first
            output bus of the seven-segment converter, using input_bus[0] and
            input_bus[3] as the most and least significant bit, respectively.
            output_bus_1[0] and output_bus_1[7] correspond to segment G and
            segment A, respectively.
        output_bus_2: An object of type BusSevenSegmentDisplay. The second
            output bus of the seven-segment converter, using input_bus[4] and
            input_bus[7] as the most and least significant bit, respectively.
            output_bus_2[0] and output_bus_2[7] correspond to segment G and
            segment A, respectively.
        output_bus_3: An object of type BusSevenSegmentDisplay. The third
            output bus of the seven-segment converter, using input_bus[8] and
            input_bus[11] as the most and least significant bit, respectively.
            output_bus_3[0] and output_bus_3[7] correspond to segment G and
            segment A, respectively.
        output_bus_4: An object of type BusSevenSegmentDisplay. The fourth
            output bus of the seven-segment converter, using input_bus[12] and
            input_bus[15] as the most and least significant bit, respectively.
            output_bus_4[0] and output_bus_4[7] correspond to segment G and
            segment A, respectively.

    Raises:
        TypeError: If input_bus is not a bus of width 16, or if either
            output_bus_1, output_bus_2, output_bus_3, or output_bus_4 is not a
            bus of width 7.
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

        self.enable = enable
        self.input_bus = input_bus
        self.output_bus_1 = output_bus_1
        self.output_bus_2 = output_bus_2
        self.output_bus_3 = output_bus_3
        self.output_bus_4 = output_bus_4

    def __str__(self):
        str_ = ""
        str_ += "enable: " + str(self.enable.value) + "\n"
        str_ += "input_bus: " + self.input_bus.__str__() + "\n"
        str_ += "output_bus_1: " + self.output_bus_1.__str__() + "\n"
        str_ += "output_bus_2: " + self.output_bus_2.__str__() + "\n"
        str_ += "output_bus_3: " + self.output_bus_3.__str__() + "\n"
        str_ += "output_bus_4: " + self.output_bus_4.__str__()
        return str_
