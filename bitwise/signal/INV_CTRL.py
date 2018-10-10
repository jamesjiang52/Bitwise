"""
This module defines classes that simulate controlled inverters. A controlled
inverter inverts all of its inputs when an enable input is set to 1. Otherwise,
the inputs pass through the inverter unchanged.

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
    """
    This controlled inverter has four inputs in a single 4-bit bus, an enable
    input, and four outputs in a single 4-bit bus:
                     ________
         enable ----|        |---- output_1
        input_1 ----|        |---- output_2
        input_2 ----|        |---- output_3
        input_3 ----|        |---- output_4
        input_4 ----|________|

    Output output_1 corresponds to input input_1, output_2 corresponds to
    input_2, and so on.
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
    """
    This controlled inverter has eight inputs in a single 8-bit bus, an enable
    input, and eight outputs in a single 8-bit bus:
                     ________
         enable ----|        |---- output_1
        input_1 ----|        |---- output_2
        input_2 ----|        |---- output_3
        input_3 ----|        |---- output_4
        input_4 ----|        |---- output_5
        input_5 ----|        |---- output_6
        input_6 ----|        |---- output_7
        input_7 ----|        |---- output_8
        input_8 ----|________|

    Output output_1 corresponds to input input_1, output_2 corresponds to
    input_2, and so on.
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
    """
    This controlled inverter has sixteen inputs in a single 16-bit bus, an
    enable input, and sixteen outputs in a single 16-bit bus:
                      ________
          enable ----|        |---- output_1
         input_1 ----|        |---- output_2
         input_2 ----|        |---- output_3
         input_3 ----|        |---- output_4
         input_4 ----|        |---- output_5
         input_5 ----|        |---- output_6
         input_6 ----|        |---- output_7
         input_7 ----|        |---- output_8
         input_8 ----|        |---- output_9
         input_9 ----|        |---- output_10
        input_10 ----|        |---- output_11
        input_11 ----|        |---- output_12
        input_12 ----|        |---- output_13
        input_13 ----|        |---- output_14
        input_14 ----|        |---- output_15
        input_15 ----|        |---- output_16
        input_16 ----|________|

    Output output_1 corresponds to input input_1, output_2 corresponds to
    input_2, and so on.
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
