"""
The following classes are defined:
    BitwiseNOT4
    BitwiseNOT8
    BitwiseNOT16
"""

from .. import wire
from .. import gate

Wire = wire.Wire
Bus4 = wire.Bus4
Bus8 = wire.Bus8
Bus16 = wire.Bus16


class BitwiseNOT4:
    """Construct a new 4-bit bitwise NOT circuit.

    Args:
        input_bus: An object of type Bus4. The input to the bitwise NOT
            operation.
        output_bus: An object of type Bus4. The output of the bitwise NOT
            operation.

    Raises:
        TypeError: If either input_bus or output_bus is not a bus of width 4.
    """
    def __init__(self, input_bus, output_bus):
        if len(input_bus) != 4:
            raise TypeError(
                "Expected bus of width 4, received bus of width {0}.".format(
                    len(input_bus)
                )
            )

        if len(output_bus) != 4:
            raise TypeError(
                "Expected bus of width 4, received bus of width {0}.".format(
                    len(output_bus)
                )
            )

        gate.NOTGate(input_bus[0], output_bus[0])
        gate.NOTGate(input_bus[1], output_bus[1])
        gate.NOTGate(input_bus[2], output_bus[2])
        gate.NOTGate(input_bus[3], output_bus[3])


class BitwiseNOT8:
    """Construct a new 8-bit bitwise NOT circuit.

    Args:
        input_bus: An object of type Bus8. The input to the bitwise NOT
            operation.
        output_bus: An object of type Bus8. The output of the bitwise NOT
            operation.

    Raises:
        TypeError: If either input_bus or output_bus is not a bus of width 8.
    """
    def __init__(self, input_bus, output_bus):
        if len(input_bus) != 8:
            raise TypeError(
                "Expected bus of width 8, received bus of width {0}.".format(
                    len(input_bus)
                )
            )

        if len(output_bus) != 8:
            raise TypeError(
                "Expected bus of width 8, received bus of width {0}.".format(
                    len(output_bus)
                )
            )

        gate.NOTGate(input_bus[0], output_bus[0])
        gate.NOTGate(input_bus[1], output_bus[1])
        gate.NOTGate(input_bus[2], output_bus[2])
        gate.NOTGate(input_bus[3], output_bus[3])
        gate.NOTGate(input_bus[4], output_bus[4])
        gate.NOTGate(input_bus[5], output_bus[5])
        gate.NOTGate(input_bus[6], output_bus[6])
        gate.NOTGate(input_bus[7], output_bus[7])


class BitwiseNOT16:
    """Construct a new 16-bit bitwise NOT circuit.

    Args:
        input_bus: An object of type Bus16. The input to the bitwise NOT
            operation.
        output_bus: An object of type Bus16. The output of the bitwise NOT
            operation.

    Raises:
        TypeError: If either input_bus or output_bus is not a bus of width 16.
    """
    def __init__(self, input_bus, output_bus):
        if len(input_bus) != 16:
            raise TypeError(
                "Expected bus of width 16, received bus of width {0}.".format(
                    len(input_bus)
                )
            )

        if len(output_bus) != 16:
            raise TypeError(
                "Expected bus of width 16, received bus of width {0}.".format(
                    len(output_bus)
                )
            )

        gate.NOTGate(input_bus[0], output_bus[0])
        gate.NOTGate(input_bus[1], output_bus[1])
        gate.NOTGate(input_bus[2], output_bus[2])
        gate.NOTGate(input_bus[3], output_bus[3])
        gate.NOTGate(input_bus[4], output_bus[4])
        gate.NOTGate(input_bus[5], output_bus[5])
        gate.NOTGate(input_bus[6], output_bus[6])
        gate.NOTGate(input_bus[7], output_bus[7])
        gate.NOTGate(input_bus[8], output_bus[8])
        gate.NOTGate(input_bus[9], output_bus[9])
        gate.NOTGate(input_bus[10], output_bus[10])
        gate.NOTGate(input_bus[11], output_bus[11])
        gate.NOTGate(input_bus[12], output_bus[12])
        gate.NOTGate(input_bus[13], output_bus[13])
        gate.NOTGate(input_bus[14], output_bus[14])
        gate.NOTGate(input_bus[15], output_bus[15])
