"""
The following classes are defined:
    BufferBus4
    BufferBus8
    BufferBus16
"""

from . import BUS
from . import TRI

Bus4 = BUS.Bus4
Bus8 = BUS.Bus8
Bus16 = BUS.Bus16
Tri = TRI.TristateBuffer


class BufferBus4:
    """Initialize a new tri-state buffer with buses of width 4 as input and
    output.

    Args:
        enable: An object of type Wire.
        input_bus: An object of type Bus4.
        output_bus: An object of type Bus4. Takes on the value of input_bus if
            enable has value 1. Otherwise, value is independent of input_bus.

    Raises:
        TypeError: If either input_bus or output_bus is not a bus of width 4.
    """
    def __init__(self, enable, input_bus, output_bus):
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

        Tri(enable, input_bus[0], output_bus[0])
        Tri(enable, input_bus[1], output_bus[1])
        Tri(enable, input_bus[2], output_bus[2])
        Tri(enable, input_bus[3], output_bus[3])


class BufferBus8:
    """Initialize a new tri-state buffer with buses of width 8 as input and
    output.

    Args:
        enable: An object of type Wire.
        input_bus: An object of type Bus8.
        output_bus: An object of type Bus8. Takes on the value of input_bus if
            enable has value 1. Otherwise, value is independent of input_bus.

    Raises:
        TypeError: If either input_bus or output_bus is not a bus of width 8.
    """
    def __init__(self, enable, input_bus, output_bus):
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

        Tri(enable, input_bus[0], output_bus[0])
        Tri(enable, input_bus[1], output_bus[1])
        Tri(enable, input_bus[2], output_bus[2])
        Tri(enable, input_bus[3], output_bus[3])
        Tri(enable, input_bus[4], output_bus[4])
        Tri(enable, input_bus[5], output_bus[5])
        Tri(enable, input_bus[6], output_bus[6])
        Tri(enable, input_bus[7], output_bus[7])


class BufferBus16:
    """Initialize a new tri-state buffer with buses of width 16 as input and
    output.

    Args:
        enable: An object of type Wire.
        input_bus: An object of type Bus16.
        output_bus: An object of type Bus16. Takes on the value of input_bus if
            enable has value 1. Otherwise, value is independent of input_bus.

    Raises:
        TypeError: If either input_bus or output_bus is not a bus of width 16.
    """
    def __init__(self, enable, input_bus, output_bus):
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

        Tri(enable, input_bus[0], output_bus[0])
        Tri(enable, input_bus[1], output_bus[1])
        Tri(enable, input_bus[2], output_bus[2])
        Tri(enable, input_bus[3], output_bus[3])
        Tri(enable, input_bus[4], output_bus[4])
        Tri(enable, input_bus[5], output_bus[5])
        Tri(enable, input_bus[6], output_bus[6])
        Tri(enable, input_bus[7], output_bus[7])
        Tri(enable, input_bus[8], output_bus[8])
        Tri(enable, input_bus[9], output_bus[9])
        Tri(enable, input_bus[10], output_bus[10])
        Tri(enable, input_bus[11], output_bus[11])
        Tri(enable, input_bus[12], output_bus[12])
        Tri(enable, input_bus[13], output_bus[13])
        Tri(enable, input_bus[14], output_bus[14])
        Tri(enable, input_bus[15], output_bus[15])
