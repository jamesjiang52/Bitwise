"""
The following classes are defined:
    Demultiplexer1To2
    Demultiplexer1To4
    Demultiplexer1To8
    Demultiplexer1To16
"""

from .. import wire
from .. import gate
from . import DEC

Wire = wire.Wire
Bus4 = wire.Bus4
Bus8 = wire.Bus8
Bus16 = wire.Bus16


class Demultiplexer1To2:
    """Construct a new 1-to-2 demultiplexer.

    Args:
        enable: An object of type Wire. Enables the demultiplexer.
        select: An object of type Wire. The select input.
        input_: An object of type Wire. The data input to the demultiplexer.
        output_1: An object of type Wire. Transmits the value of input_ if the
            value of select is 1.
        output_2: An object of type Wire. Transmits the value of input_ if the
            value of select is 0.
    """
    def __init__(self, enable, select, input_1, output_1, output_2):
        wire_1 = Wire()

        gate.NOTGate(select, wire_1)
        gate.ANDGate3(enable, select, input_1, output_1)
        gate.ANDGate3(enable, wire_1, input_1, output_2)


class Demultiplexer1To4:
    """Construct a new 1-to-4 demultiplexer.

    Args:
        enable: An object of type Wire. Enables the demultiplexer.
        select_1: An object of type Wire. The most significant bit of the
            select input.
        select_2: An object of type Wire. The least significant bit of the
            select input.
        input_: An object of type Wire. The data input to the demultiplexer.
        output_bus: An object of type Bus4. output[0] transmits the value of
            input_ for a (1, 1) select, and output[3] transmits the value of
            input_ for a (0, 0) select.

    Raises:
        TypeError: If output_bus is not a bus of width 4.
    """
    def __init__(self, enable, select_1, select_2, input_1, output_bus):
        if len(output_bus.wires) != 4:
            raise TypeError(
                "Expected bus of width 4, received bus of width {0}.".format(
                    len(output_bus.wires)
                )
            )

        wire_1 = Wire()
        wire_2 = Wire()
        wire_3 = Wire()
        wire_4 = Wire()
        bus_1 = Bus4(wire_1, wire_2, wire_3, wire_4)

        DEC.Decoder1Of4(input_1, select_1, select_2, bus_1)
        gate.ANDGate2(enable, wire_1, output_bus.wires[0])
        gate.ANDGate2(enable, wire_2, output_bus.wires[1])
        gate.ANDGate2(enable, wire_3, output_bus.wires[2])
        gate.ANDGate2(enable, wire_4, output_bus.wires[3])


class Demultiplexer1To8:
    """
    Construct a new 1-to-8 demultiplexer.

    Args:
        enable: An object of type Wire. Enables the demultiplexer.
        select_1: An object of type Wire. The most significant bit of the
            select input.
        select_2: An object of type Wire.
        select_3: An object of type Wire. The least significant bit of the
            select input.
        input_: An object of type Wire. The data input to the demultiplexer.
        output_bus: An object of type Bus8. output[0] transmits the value of
            input_ for a (1, 1, 1) select, and output[7] transmits the value of
            input_ for a (0, 0, 0) select.

    Raises:
        TypeError: If output_bus is not a bus of width 8.
    """
    def __init__(
            self,
            enable,
            select_1,
            select_2,
            select_3,
            input_1,
            output_bus
            ):
        if len(output_bus.wires) != 8:
            raise TypeError(
                "Expected bus of width 8, received bus of width {0}.".format(
                    len(output_bus.wires)
                )
            )

        wire_1 = Wire()
        wire_2 = Wire()
        wire_3 = Wire()
        wire_4 = Wire()
        wire_5 = Wire()
        wire_6 = Wire()
        wire_7 = Wire()
        wire_8 = Wire()
        bus_1 = Bus8(
            wire_1,
            wire_2,
            wire_3,
            wire_4,
            wire_5,
            wire_6,
            wire_7,
            wire_8
        )

        DEC.Decoder1Of8(input_1, select_1, select_2, select_3, bus_1)
        gate.ANDGate2(enable, wire_1, output_bus.wires[0])
        gate.ANDGate2(enable, wire_2, output_bus.wires[1])
        gate.ANDGate2(enable, wire_3, output_bus.wires[2])
        gate.ANDGate2(enable, wire_4, output_bus.wires[3])
        gate.ANDGate2(enable, wire_5, output_bus.wires[4])
        gate.ANDGate2(enable, wire_6, output_bus.wires[5])
        gate.ANDGate2(enable, wire_7, output_bus.wires[6])
        gate.ANDGate2(enable, wire_8, output_bus.wires[7])


class Demultiplexer1To16:
    """Construct a new 1-to-16 demultiplexer.

    Args:
        enable: An object of type Wire. Enables the demultiplexer.
        select_bus: An object of type Bus4. The select input to the
            demultiplexer. select[0] and select[3] are the most and least
            significant bit, respectively.
        input_: An object of type Wire. The data input to the demultiplexer.
        output_bus: An object of type Bus16. output[0] transmits the value of
            input_ for a (1, 1, 1, 1) select, and output[15] transmits the
            value of input_ for a (0, 0, 0, 0) select.

    Raises:
        TypeError: If select_bus is not a bus of width 4, or if output_bus is
        not a bus of width 16.
    """
    def __init__(self, enable, select_bus, input_1, output_bus):
        if len(select_bus.wires) != 4:
            raise TypeError(
                "Expected bus of width 4, received bus of width {0}.".format(
                    len(select_bus.wires)
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
        wire_5 = Wire()
        wire_6 = Wire()
        wire_7 = Wire()
        wire_8 = Wire()
        wire_9 = Wire()
        wire_10 = Wire()
        wire_11 = Wire()
        wire_12 = Wire()
        wire_13 = Wire()
        wire_14 = Wire()
        wire_15 = Wire()
        wire_16 = Wire()
        bus_1 = Bus16(
            wire_1,
            wire_2,
            wire_3,
            wire_4,
            wire_5,
            wire_6,
            wire_7,
            wire_8,
            wire_9,
            wire_10,
            wire_11,
            wire_12,
            wire_13,
            wire_14,
            wire_15,
            wire_16
        )

        DEC.Decoder1Of16(input_1, select_bus, bus_1)
        gate.ANDGate2(enable, wire_1, output_bus.wires[0])
        gate.ANDGate2(enable, wire_2, output_bus.wires[1])
        gate.ANDGate2(enable, wire_3, output_bus.wires[2])
        gate.ANDGate2(enable, wire_4, output_bus.wires[3])
        gate.ANDGate2(enable, wire_5, output_bus.wires[4])
        gate.ANDGate2(enable, wire_6, output_bus.wires[5])
        gate.ANDGate2(enable, wire_7, output_bus.wires[6])
        gate.ANDGate2(enable, wire_8, output_bus.wires[7])
        gate.ANDGate2(enable, wire_9, output_bus.wires[8])
        gate.ANDGate2(enable, wire_10, output_bus.wires[9])
        gate.ANDGate2(enable, wire_11, output_bus.wires[10])
        gate.ANDGate2(enable, wire_12, output_bus.wires[11])
        gate.ANDGate2(enable, wire_13, output_bus.wires[12])
        gate.ANDGate2(enable, wire_14, output_bus.wires[13])
        gate.ANDGate2(enable, wire_15, output_bus.wires[14])
        gate.ANDGate2(enable, wire_16, output_bus.wires[15])
