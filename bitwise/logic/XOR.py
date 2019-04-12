"""
The following classes are defined:
    BitwiseXOR4
    BitwiseXOR8
    BitwiseXOR16
"""

from .. import wire
from .. import gate

Wire = wire.Wire
Bus4 = wire.Bus4
Bus8 = wire.Bus8
Bus16 = wire.Bus16


class BitwiseXOR4:
    """Construct a new 4-bit bitwise XOR circuit.

    Args:
        a_bus: An object of type Bus4. The first input.
        b_bus: An object of type Bus4. The second input.
        output_bus: An object of type Bus4. The output of the bitwise XOR
            operation.

    Raises:
        TypeError: If either a_bus, b_bus, or output_bus is not a bus of width
            4.
    """
    def __init__(self, a_bus, b_bus, output_bus):
        if len(a_bus) != 4:
            raise TypeError(
                "Expected bus of width 4, received bus of width {0}.".format(
                    len(a_bus)
                )
            )

        if len(b_bus) != 4:
            raise TypeError(
                "Expected bus of width 4, received bus of width {0}.".format(
                    len(b_bus)
                )
            )

        if len(output_bus) != 4:
            raise TypeError(
                "Expected bus of width 4, received bus of width {0}.".format(
                    len(output_bus)
                )
            )

        gate.XORGate2(a_bus[0], b_bus[0], output_bus[0])
        gate.XORGate2(a_bus[1], b_bus[1], output_bus[1])
        gate.XORGate2(a_bus[2], b_bus[2], output_bus[2])
        gate.XORGate2(a_bus[3], b_bus[3], output_bus[3])

        self.a_bus = a_bus
        self.b_bus = b_bus
        self.output_bus = output_bus

    def __str__(self):
        str_ = ""
        str_ += "a_bus: " + self.a_bus.__str__() + "\n"
        str_ += "b_bus: " + self.b_bus.__str__() + "\n"
        str_ += "output_bus: " + self.output_bus.__str__()
        return str_


class BitwiseXOR8:
    """Construct a new 8-bit bitwise XOR circuit.

    Args:
        a_bus: An object of type Bus8. The first input.
        b_bus: An object of type Bus8. The second input.
        output_bus: An object of type Bus8. The output of the bitwise XOR
            operation.

    Raises:
        TypeError: If either a_bus, b_bus, or output_bus is not a bus of width
            8.
    """
    def __init__(self, a_bus, b_bus, output_bus):
        if len(a_bus) != 8:
            raise TypeError(
                "Expected bus of width 8, received bus of width {0}.".format(
                    len(a_bus)
                )
            )

        if len(b_bus) != 8:
            raise TypeError(
                "Expected bus of width 8, received bus of width {0}.".format(
                    len(b_bus)
                )
            )

        if len(output_bus) != 8:
            raise TypeError(
                "Expected bus of width 8, received bus of width {0}.".format(
                    len(output_bus)
                )
            )

        gate.XORGate2(a_bus[0], b_bus[0], output_bus[0])
        gate.XORGate2(a_bus[1], b_bus[1], output_bus[1])
        gate.XORGate2(a_bus[2], b_bus[2], output_bus[2])
        gate.XORGate2(a_bus[3], b_bus[3], output_bus[3])
        gate.XORGate2(a_bus[4], b_bus[4], output_bus[4])
        gate.XORGate2(a_bus[5], b_bus[5], output_bus[5])
        gate.XORGate2(a_bus[6], b_bus[6], output_bus[6])
        gate.XORGate2(a_bus[7], b_bus[7], output_bus[7])

        self.a_bus = a_bus
        self.b_bus = b_bus
        self.output_bus = output_bus

    def __str__(self):
        str_ = ""
        str_ += "a_bus: " + self.a_bus.__str__() + "\n"
        str_ += "b_bus: " + self.b_bus.__str__() + "\n"
        str_ += "output_bus: " + self.output_bus.__str__()
        return str_


class BitwiseXOR16:
    """Construct a new 16-bit bitwise XOR circuit.

    Args:
        a_bus: An object of type Bus16. The first input.
        b_bus: An object of type Bus16. The second input.
        output_bus: An object of type Bus16. The output of the bitwise XOR
            operation.

    Raises:
        TypeError: If either a_bus, b_bus, or output_bus is not a bus of width
            16.
    """
    def __init__(self, a_bus, b_bus, output_bus):
        if len(a_bus) != 16:
            raise TypeError(
                "Expected bus of width 16, received bus of width {0}.".format(
                    len(a_bus)
                )
            )

        if len(b_bus) != 16:
            raise TypeError(
                "Expected bus of width 16, received bus of width {0}.".format(
                    len(b_bus)
                )
            )

        if len(output_bus) != 16:
            raise TypeError(
                "Expected bus of width 16, received bus of width {0}.".format(
                    len(output_bus)
                )
            )

        gate.XORGate2(a_bus[0], b_bus[0], output_bus[0])
        gate.XORGate2(a_bus[1], b_bus[1], output_bus[1])
        gate.XORGate2(a_bus[2], b_bus[2], output_bus[2])
        gate.XORGate2(a_bus[3], b_bus[3], output_bus[3])
        gate.XORGate2(a_bus[4], b_bus[4], output_bus[4])
        gate.XORGate2(a_bus[5], b_bus[5], output_bus[5])
        gate.XORGate2(a_bus[6], b_bus[6], output_bus[6])
        gate.XORGate2(a_bus[7], b_bus[7], output_bus[7])
        gate.XORGate2(a_bus[8], b_bus[8], output_bus[8])
        gate.XORGate2(a_bus[9], b_bus[9], output_bus[9])
        gate.XORGate2(a_bus[10], b_bus[10], output_bus[10])
        gate.XORGate2(a_bus[11], b_bus[11], output_bus[11])
        gate.XORGate2(a_bus[12], b_bus[12], output_bus[12])
        gate.XORGate2(a_bus[13], b_bus[13], output_bus[13])
        gate.XORGate2(a_bus[14], b_bus[14], output_bus[14])
        gate.XORGate2(a_bus[15], b_bus[15], output_bus[15])

        self.a_bus = a_bus
        self.b_bus = b_bus
        self.output_bus = output_bus

    def __str__(self):
        str_ = ""
        str_ += "a_bus: " + self.a_bus.__str__() + "\n"
        str_ += "b_bus: " + self.b_bus.__str__() + "\n"
        str_ += "output_bus: " + self.output_bus.__str__()
        return str_
