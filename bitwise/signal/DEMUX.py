"""
This module defines classes that simulate demultiplexers. A demultiplexer
receives a single input and selects one output to transmit the input to. This
selection is done by one or more select inputs. The other outputs that are not
selected will have the value 0. The demultiplexers in this module have an
additional enable input; if this input is 0, all the output values are 0,
regardless of the other inputs.

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
    """
    This demultiplexer has a data input, a single select input, an enable
    input, and two outputs:
                    ________
        enable ----|        |---- output_1
        select ----|        |---- output_2
         input ----|________|

    The input is transmitted to output_1 for a (1) select and output_2 for a
    (0) select. If the enable is 0, the outputs are all 0, regardless of input.
    """
    def __init__(self, enable, select, input_1, output_1, output_2):
        wire_1 = Wire()

        gate.NOTGate(select, wire_1)
        gate.ANDGate3(enable, select, input_1, output_1)
        gate.ANDGate3(enable, wire_1, input_1, output_2)


class Demultiplexer1To4:
    """
    This demultiplexer has a data input, two select inputs, an enable input,
    and four outputs in a single 4-bit bus:
                      ________
          enable ----|        |---- output_1
        select_1 ----|        |---- output_2
        select_2 ----|        |---- output_3
           input ----|________|---- output_4

    The selects have select_1 and select_2 as the MSB and LSB, respectively.
    The input is transmitted to output_1 for a (1, 1) select and output_4 for a
    (0, 0) select. If the enable is 0, the outputs are all 0, regardless of
    input.
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
    This demultiplexer has a data input, three select inputs, an enable input,
    and eight outputs in a single 8-bit bus:
                      ________
          enable ----|        |---- output_1
        select_1 ----|        |---- output_2
        select_2 ----|        |---- output_3
        select_3 ----|        |---- output_4
           input ----|        |---- output_5
                     |        |---- output_6
                     |        |---- output_7
                     |________|---- output_8

    The selects have select_1 and select_3 as the MSB and LSB, respectively.
    The input is transmitted to output_1 for a (1, 1, 1) select and output_8
    for a (0, 0, 0) select. If the enable is 0, the outputs are all 0,
    regardless of input.
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
    """
    This demultiplexer has a data input, four select inputs in a single 4-bit
    bus, an enable input, and sixteen outputs in a single 16-bit bus:
                      ________
          enable ----|        |---- output_1
        select_1 ----|        |---- output_2
        select_2 ----|        |---- output_3
        select_3 ----|        |---- output_4
        select_4 ----|        |---- output_5
           input ----|        |---- output_6
                     |        |---- output_7
                     |        |---- output_8
                     |        |---- output_9
                     |        |---- output_10
                     |        |---- output_11
                     |        |---- output_12
                     |        |---- output_13
                     |        |---- output_14
                     |        |---- output_15
                     |________|---- output_16

    The selects have select_1 and select_4 as the MSB and LSB, respectively.
    The input is transmitted to output_1 for a (1, 1, 1, 1) select and
    output_16 for a (0, 0, 0, 0) select. If the enable is 0, the outputs are
    all 0, regardless of input.
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
