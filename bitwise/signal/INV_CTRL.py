"""
The following classes are defined:
    ControlledInverter4
    ControlledInverter8
    ControlledInverter16
"""

from .. import wire
from .. import gate

Wire = wire.Wire
Bus4 = wire.Bus4
Bus8 = wire.Bus8
Bus16 = wire.Bus16


class ControlledInverter4:
    """Construct a new 4-bit controlled inverter.

    Args:
        enable: An object of type Wire. Enables the controlled inverter.
        input_bus: An object of type Bus4. The data input to the controlled
            inverter.
        output_bus: An object of type Bus4. The output of the controlled
            inverter, which is the inverted form of the data input iff enable
            has value 1.

    Raises:
        TypeError: If either input_bus or output_bus is not a bus of width 4.
    """
    def __init__(self, enable, input_bus, output_bus):
        if len(input_bus.wires) != 4:
            raise TypeError(
                "Expected bus of width 4, received bus of width {0}.".format(
                    len(input_bus.wires)
                )
            )

        if len(output_bus.wires) != 4:
            raise TypeError(
                "Expected bus of width 4, received bus of width {0}.".format(
                    len(output_bus.wires)
                )
            )

        gate.XORGate2(enable, input_bus.wires[0], output_bus.wires[0])
        gate.XORGate2(enable, input_bus.wires[1], output_bus.wires[1])
        gate.XORGate2(enable, input_bus.wires[2], output_bus.wires[2])
        gate.XORGate2(enable, input_bus.wires[3], output_bus.wires[3])


class ControlledInverter8:
    """Construct a new 8-bit controlled inverter.

    Args:
        enable: An object of type Wire. Enables the controlled inverter.
        input_bus: An object of type Bus8. The data input to the controlled
            inverter.
        output_bus: An object of type Bus8. The output of the controlled
            inverter, which is the inverted form of the data input iff enable
            has value 1.

    Raises:
        TypeError: If either input_bus or output_bus is not a bus of width 8.
    """
    def __init__(self, enable, input_bus, output_bus):
        if len(input_bus.wires) != 8:
            raise TypeError(
                "Expected bus of width 8, received bus of width {0}.".format(
                    len(input_bus.wires)
                )
            )

        if len(output_bus.wires) != 8:
            raise TypeError(
                "Expected bus of width 8, received bus of width {0}.".format(
                    len(output_bus.wires)
                )
            )

        bus_1 = Bus4(*input_bus.wires[0:4])
        bus_2 = Bus4(*input_bus.wires[4:8])
        bus_3 = Bus4(*output_bus.wires[0:4])
        bus_4 = Bus4(*output_bus.wires[4:8])

        ControlledInverter4(enable, bus_1, bus_3)
        ControlledInverter4(enable, bus_2, bus_4)


class ControlledInverter16:
    """Construct a new 16-bit controlled inverter.

    Args:
        enable: An object of type Wire. Enables the controlled inverter.
        input_bus: An object of type Bus16. The data input to the controlled
            inverter.
        output_bus: An object of type Bus16. The output of the controlled
            inverter, which is the inverted form of the data input iff enable
            has value 1.

    Raises:
        TypeError: If either input_bus or output_bus is not a bus of width 16.
    """
    def __init__(self, enable, input_bus, output_bus):
        if len(input_bus.wires) != 16:
            raise TypeError(
                "Expected bus of width 16, received bus of width {0}.".format(
                    len(input_bus.wires)
                )
            )

        if len(output_bus.wires) != 16:
            raise TypeError(
                "Expected bus of width 16, received bus of width {0}.".format(
                    len(output_bus.wires)
                )
            )

        bus_1 = Bus8(*input_bus.wires[0:8])
        bus_2 = Bus8(*input_bus.wires[8:16])
        bus_3 = Bus8(*output_bus.wires[0:8])
        bus_4 = Bus8(*output_bus.wires[8:16])

        ControlledInverter8(enable, bus_1, bus_3)
        ControlledInverter8(enable, bus_2, bus_4)
