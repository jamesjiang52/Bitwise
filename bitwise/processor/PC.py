"""
The following classes are defined:
    ProgramCounter
"""

from .. import wire
from .. import arithmetic
from .. import gate
from .. import signal
from .. import storage

Wire = wire.Wire
Bus4 = wire.Bus4
Bus8 = wire.Bus8
Bus16 = wire.Bus16


class ProgramCounter:
    """Construct a new program counter with a 16-bit address space.

    Args:
        load_bus: An object of type Bus16.
        up: An object of type Wire. If its value is 1, increments the program
            counter on the positive clock edge.
        load: An object of type Wire. If its value is 1, loads the value of
            load_bus into the program counter on the positive clock edge. If
            both up and load have value 1, load takes precedence.
        clock: An object of type Wire or Clock. The clock input.
        output_bus: An object of type Bus16. The address of the instruction to
            be executed.
    """
    def __init__(self, input_bus, up, load, clock, output_bus):
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

        b_bus = Bus16()
        b_bus.wires[15].value = 1
        sum_ = Bus16()
        mux_out = Bus16()

        v = Wire()
        c = Wire()
        enable = Wire()
        gnd = Wire()
        gnd.value = 0

        gate.ORGate2(up, load, enable)
        _Multiplexer2To1_16(load, input_bus, sum_, mux_out)
        arithmetic.AdderSubtractor16(gnd, output_bus, b_bus, v, c, sum_)
        storage.Register16(mux_out, enable, clock, output_bus)


class _Multiplexer2To1_16:
    """
    This is an internal module for ProgramCounter. It multiplexes two 16-bit
    inputs to a single 16-bit output.
    """
    def __init__(
        self,
        select,
        input_1_bus,
        input_2_bus,
        output_bus
    ):
        vcc = Wire()
        vcc.value = 1

        signal.Multiplexer2To1(
            vcc,
            select,
            input_1_bus[0],
            input_2_bus[0],
            output_bus[0]
        )
        signal.Multiplexer2To1(
            vcc,
            select,
            input_1_bus[1],
            input_2_bus[1],
            output_bus[1]
        )
        signal.Multiplexer2To1(
            vcc,
            select,
            input_1_bus[2],
            input_2_bus[2],
            output_bus[2]
        )
        signal.Multiplexer2To1(
            vcc,
            select,
            input_1_bus[3],
            input_2_bus[3],
            output_bus[3]
        )
        signal.Multiplexer2To1(
            vcc,
            select,
            input_1_bus[4],
            input_2_bus[4],
            output_bus[4]
        )
        signal.Multiplexer2To1(
            vcc,
            select,
            input_1_bus[5],
            input_2_bus[5],
            output_bus[5]
        )
        signal.Multiplexer2To1(
            vcc,
            select,
            input_1_bus[6],
            input_2_bus[6],
            output_bus[6]
        )
        signal.Multiplexer2To1(
            vcc,
            select,
            input_1_bus[7],
            input_2_bus[7],
            output_bus[7]
        )

        signal.Multiplexer2To1(
            vcc,
            select,
            input_1_bus[8],
            input_2_bus[8],
            output_bus[8]
        )
        signal.Multiplexer2To1(
            vcc,
            select,
            input_1_bus[9],
            input_2_bus[9],
            output_bus[9]
        )
        signal.Multiplexer2To1(
            vcc,
            select,
            input_1_bus[10],
            input_2_bus[10],
            output_bus[10]
        )
        signal.Multiplexer2To1(
            vcc,
            select,
            input_1_bus[11],
            input_2_bus[11],
            output_bus[11]
        )
        signal.Multiplexer2To1(
            vcc,
            select,
            input_1_bus[12],
            input_2_bus[12],
            output_bus[12]
        )
        signal.Multiplexer2To1(
            vcc,
            select,
            input_1_bus[13],
            input_2_bus[13],
            output_bus[13]
        )
        signal.Multiplexer2To1(
            vcc,
            select,
            input_1_bus[14],
            input_2_bus[14],
            output_bus[14]
        )
        signal.Multiplexer2To1(
            vcc,
            select,
            input_1_bus[15],
            input_2_bus[15],
            output_bus[15]
        )
