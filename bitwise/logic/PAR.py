"""
The following classes are defined:
    ParityGenerator4
    ParityChecker4
    ParityGenerator8
    ParityChecker8
    ParityGenerator16
    ParityChecker16
"""

from .. import wire
from .. import gate

Wire = wire.Wire
Bus4 = wire.Bus4
Bus8 = wire.Bus8
Bus16 = wire.Bus16


class ParityGenerator4:
    """Construct a new 4-bit even parity generator.

    Args:
        input_bus: An object of type Bus4. The input to the parity generator.
        parity_bit: An object of type Wire. The parity bit.

    Raises:
        TypeError: If input_bus is not a bus of width 4.
    """
    def __init__(self, input_bus, parity_bit):
        if len(input_bus.wires) != 4:
            raise TypeError(
                "Expected bus of width 4, received bus of width {0}.".format(
                    len(input_bus.wires)
                )
            )

        wire_1 = Wire()
        wire_2 = Wire()

        gate.XORGate2(input_bus.wires[0], input_bus.wires[1], wire_1)
        gate.XORGate2(input_bus.wires[2], input_bus.wires[3], wire_2)
        gate.XORGate2(wire_1, wire_2, parity_bit)


class ParityChecker4:
    """Construct a new 4-bit even parity checker.

    Args:
        input_bus: An object of type Bus4. The input to the parity checker.
        parity_bit: An object of type Wire. The parity bit.
        error: An object of type Wire. The error indicator.

    Raises:
        TypeError: If input_bus is not a bus of width 4.
    """
    def __init__(self, input_bus, parity_bit, error):
        if len(input_bus.wires) != 4:
            raise TypeError(
                "Expected bus of width 4, received bus of width {0}.".format(
                    len(input_bus.wires)
                )
            )

        wire_1 = Wire()
        ParityGenerator4(input_bus, wire_1)
        gate.XORGate2(wire_1, parity_bit, error)


class ParityGenerator8:
    """Construct a new 8-bit even parity generator.

    Args:
        input_bus: An object of type Bus8. The input to the parity generator.
        parity_bit: An object of type Wire. The parity bit.

    Raises:
        TypeError: If input_bus is not a bus of width 8.
    """
    def __init__(self, input_bus, parity_bit):
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

        ParityGenerator4(bus_1, wire_1)
        ParityGenerator4(bus_2, wire_2)
        gate.XORGate2(wire_1, wire_2, parity_bit)


class ParityChecker8:
    """Construct a new 8-bit even parity checker.

    Args:
        input_bus: An object of type Bus8. The input to the parity checker.
        parity_bit: An object of type Wire. The parity bit.
        error: An object of type Wire. The error indicator.

    Raises:
        TypeError: If input_bus is not a bus of width 8.
    """
    def __init__(self, input_bus, parity_bit, error):
        if len(input_bus.wires) != 8:
            raise TypeError(
                "Expected bus of width 8, received bus of width {0}.".format(
                    len(input_bus.wires)
                )
            )

        wire_1 = Wire()
        ParityGenerator8(input_bus, wire_1)
        gate.XORGate2(wire_1, parity_bit, error)


class ParityGenerator16:
    """Construct a new 16-bit even parity generator.

    Args:
        input_bus: An object of type Bus16. The input to the parity generator.
        parity_bit: An object of type Wire. The parity bit.

    Raises:
        TypeError: If input_bus is not a bus of width 16.
    """
    def __init__(self, input_bus, parity_bit):
        if len(input_bus.wires) != 16:
            raise TypeError(
                "Expected bus of width 16, received bus of width {0}.".format(
                    len(input_bus.wires)
                )
            )

        wire_1 = Wire()
        wire_2 = Wire()
        bus_1 = Bus8(*input_bus.wires[0:8])
        bus_2 = Bus8(*input_bus.wires[8:16])

        ParityGenerator8(bus_1, wire_1)
        ParityGenerator8(bus_2, wire_2)
        gate.XORGate2(wire_1, wire_2, parity_bit)


class ParityChecker16:
    """Construct a new 16-bit even parity checker.

    Args:
        input_bus: An object of type Bus16. The input to the parity checker.
        parity_bit: An object of type Wire. The parity bit.
        error: An object of type Wire. The error indicator.

    Raises:
        TypeError: If input_bus is not a bus of width 16.
    """
    def __init__(self, input_bus, parity_bit, error):
        if len(input_bus.wires) != 16:
            raise TypeError(
                "Expected bus of width 16, received bus of width {0}.".format(
                    len(input_bus.wires)
                )
            )

        wire_1 = Wire()
        ParityGenerator16(input_bus, wire_1)
        gate.XORGate2(wire_1, parity_bit, error)
