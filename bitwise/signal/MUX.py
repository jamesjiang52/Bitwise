"""
This module defines classes that simulate multiplexers. A multiplexer receives
multiple inputs and selects one of them to be the output. This selection is
done by one or more select inputs. The multiplexers in this module have an
additional enable input; if this input is 0, all the output values are 0,
regardless of the other inputs.

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
    """
    This multiplexer has two data inputs, an enable input, a single select
    input, and a single output:
                     ________
         enable ----|        |---- output
         select ----|        |
        input_1 ----|        |
        input_2 ----|________|

    The output takes on the value of input_2 for a (0) select and input_1 for a
    (1) select. If the enable is 0, the outputs are all 0, regardless of input.
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


class Multiplexer4To1:
    """
    This multiplexer has four data inputs in a single 4-bit bus, an enable
    input, two select inputs, and a single output:
                      ________
          enable ----|        |---- output
        select_1 ----|        |
        select_2 ----|        |
         input_1 ----|        |
         input_2 ----|        |
         input_3 ----|        |
         input_4 ----|________|

    The selects have select_1 and select_2 as the MSB and LSB, respectively.
    The output takes on the value of input_4 for a (0, 0) select and input_1
    for a (1, 1) select. If the enable is 0, the output is 0, regardless of
    input.
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


class Multiplexer8To1:
    """
    This multiplexer has eight data inputs in a single 8-bit bus, an enable
    input, three select inputs, and a single output:
                      ________
          enable ----|        |---- output
        select_1 ----|        |
        select_2 ----|        |
        select_3 ----|        |
         input_1 ----|        |
         input_2 ----|        |
         input_3 ----|        |
         input_4 ----|        |
         input_5 ----|        |
         input_6 ----|        |
         input_7 ----|        |
         input_8 ----|________|

    The selects have select_1 and select_3 as the MSB and LSB, respectively.
    The output takes on the value of input_8 for a (0, 0, 0) select and input_1
    for a (1, 1, 1) select. If the enable is 0, the output is 0, regardless of
    input.
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


class Multiplexer16To1:
    """
    This multiplexer has sixteen data inputs in a single 16-bit bus, an enable
    input, four select inputs in a single 4-bit bus, and a single output:
                      ________
          enable ----|        |---- output
        select_1 ----|        |
        select_2 ----|        |
        select_3 ----|        |
        select_4 ----|        |
         input_1 ----|        |
         input_2 ----|        |
         input_3 ----|        |
         input_4 ----|        |
         input_5 ----|        |
         input_6 ----|        |
         input_7 ----|        |
         input_8 ----|        |
         input_9 ----|        |
        input_10 ----|        |
        input_11 ----|        |
        input_12 ----|        |
        input_13 ----|        |
        input_14 ----|        |
        input_15 ----|        |
        input_16 ----|________|

    The selects have select_1 and select_4 as the MSB and LSB, respectively.
    The output takes on the value of input_16 for a (0, 0, 0, 0) select and
    input_1 for a (1, 1, 1, 1) select. If the enable is 0, the output is 0,
    regardless of input.
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
