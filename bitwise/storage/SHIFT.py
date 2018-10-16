"""

"""

from .. import wire
from .. import signal
from . import FLOP

Wire = wire.Wire
Bus4 = wire.Bus4
Bus8 = wire.Bus8
Bus16 = wire.Bus16


class SerialToParallelConverter1To4:
    """

    """
    def __init__(self, reset_n, data, clock, output_bus):
        if len(output_bus.wires) != 4:
            raise TypeError(
                "Expected bus of width 4, received bus of width {0}.".format(
                    len(output_bus.wires)
                )
            )

        output_1_not = Wire()
        output_2_not = Wire()
        output_3_not = Wire()
        output_4_not = Wire()
        preset_n = Wire()
        preset_n.value = 1

        FLOP.DFlipFlopPresetClear(
            data,
            preset_n,
            reset_n,
            clock,
            output_bus.wires[0],
            output_1_not
        )
        FLOP.DFlipFlopPresetClear(
            output_bus.wires[0],
            preset_n,
            reset_n,
            clock,
            output_bus.wires[1],
            output_2_not
        )
        FLOP.DFlipFlopPresetClear(
            output_bus.wires[1],
            preset_n,
            reset_n,
            clock,
            output_bus.wires[2],
            output_3_not
        )
        FLOP.DFlipFlopPresetClear(
            output_bus.wires[2],
            preset_n,
            reset_n,
            clock,
            output_bus.wires[3],
            output_4_not
        )


class SerialToParallelConverter1To8:
    """

    """
    def __init__(self, reset_n, data, clock, output_bus):
        if len(output_bus.wires) != 8:
            raise TypeError(
                "Expected bus of width 8, received bus of width {0}.".format(
                    len(output_bus.wires)
                )
            )

        output = output_bus.wires

        bus_1 = Bus4(*output[0:4])
        bus_2 = Bus4(*output[4:8])
        SerialToParallelConverter1To4(reset_n, data, clock, bus_1)
        SerialToParallelConverter1To4(reset_n, output[3], clock, bus_2)


class SerialToParallelConverter1To16:
    """

    """
    def __init__(self, reset_n, data, clock, output_bus):
        if len(output_bus.wires) != 16:
            raise TypeError(
                "Expected bus of width 16, received bus of width {0}.".format(
                    len(output_bus.wires)
                )
            )

        output = output_bus.wires

        bus_1 = Bus8(*output[0:8])
        bus_2 = Bus8(*output[8:16])
        SerialToParallelConverter1To8(reset_n, data, clock, bus_1)
        SerialToParallelConverter1To8(reset_n, output[7], clock, bus_2)


class ParallelToSerialConverter4To1:
    """

    """
    def __init__(self, reset_n, parallel_load_n, data_bus, clock, output):
        if len(data_bus.wires) != 4:
            raise TypeError(
                "Expected bus of width 4, received bus of width {0}.".format(
                    len(data_bus.wires)
                )
            )

        gnd = Wire()
        gnd.value = 0
        vcc = Wire()
        vcc.value = 1

        mux_1_out = Wire()
        mux_2_out = Wire()
        mux_3_out = Wire()
        mux_4_out = Wire()

        signal.Multiplexer2To1(vcc, parallel_load_n, gnd, data_bus.wires[0])
        signal.Multiplexer2To1
