"""
The following classes are defined:
    SRLatch
    GatedSRLatch
    GatedDLatch
    DFlipFlop
    DFlipFlopPresetClear
    TFlipFlop
    TFlipFlopPresetClear
    JKFlipFlop
    JKFlipFlopPresetClear
"""

from .. import wire
from .. import gate
from .. import signal

Wire = wire.Wire


class SRLatch:
    """Construct a new SR latch.

    Args:
        set_: An object of type Wire. The set input to the latch.
        reset: An object of type Wire. The reset input to the latch.
        output: An object of type Wire. The output of the latch. Takes on the
            value of 1 if the value of set is 1 and the value of 0 if the value
            of reset is 1.
        output_not: An object of type Wire. The complemented form of output.
    """
    def __init__(self, set_, reset, output, output_not):
        gate.NORGate2(set_, output, output_not)
        gate.NORGate2(reset, output_not, output)


class GatedSRLatch:
    """Construct a new gated SR latch.

    Args:
        set_: An object of type Wire. The set input to the latch.
        reset: An object of type Wire. The reset input to the latch.
        clock: An object of type Wire or Clock. The clock input to the latch.
        output: An object of type Wire. The output of the latch. When the value
            of clock is 1, takes on the value of 1 if the value of set is 1 and
            the value of 0 if the value of reset is 1.
        output_not: An object of type Wire. The complemented form of output.
    """
    def __init__(self, set_, reset, clock, output, output_not):
        wire_1 = Wire()
        wire_2 = Wire()

        gate.ANDGate2(clock, set_, wire_1)
        gate.ANDGate2(clock, reset, wire_2)
        SRLatch(wire_1, wire_2, output, output_not)


class GatedDLatch:
    """Construct a new gated D latch.

    Args:
        data: An object of type Wire. The data input to the latch.
        clock: An object of type Wire or Clock. The clock input to the latch.
        output: An object of type Wire. The output of the latch. Takes on the
            value of data if the value of clock is 1.
        output_not: An object of type Wire. The complemented form of output.
    """
    def __init__(self, data, clock, output, output_not):
        wire_1 = Wire()

        gate.NOTGate(data, wire_1)
        GatedSRLatch(data, wire_1, clock, output, output_not)


class DFlipFlop:
    """Construct a new positive edge-triggered D flip-flop.

    Args:
        data: An object of type Wire. The data input to the flip-flop.
        clock: An object of type Wire or Clock. The clock input to the flip-
            flop.
        output: An object of type Wire. The output of the flip-flop. Takes on
            the value of data on the positive edges of clock.
        output_not: An object of type Wire. The complemented form of output.
    """
    def __init__(self, data, clock, output, output_not):
        q_1 = Wire()
        q_not_1 = Wire()
        not_clock = Wire()

        gate.NOTGate(clock, not_clock)
        GatedDLatch(data, not_clock, q_1, q_not_1)
        GatedDLatch(q_1, clock, output, output_not)


class DFlipFlopPresetClear:
    """Construct a new positive edge-triggered D flip-flop with preset/clear
        capabilities.

    Args:
        data: An object of type Wire. The data input to the flip-flop.
        preset_n: An object of type Wire. Presets output to 1 and output_not to
            0 asynchronously if its value is 0.
        clear_n: An object of type Wire. Clears output to 0 and output_not to 1
            asynchronously if its value is 0.
        clock: An object of type Wireor Clock. The clock input to the flip-
            flop.
        output: An object of type Wire. The output of the flip-flop. Takes on
            the value of data on the positive edges of clock.
        output_not: An object of type Wire. The complemented form of output.
    """
    def __init__(self, data, preset_n, clear_n, clock, output, output_not):
        not_clock = Wire()
        not_data = Wire()
        wire_1 = Wire()
        wire_2 = Wire()
        wire_3 = Wire()
        wire_4 = Wire()
        wire_5 = Wire()
        wire_6 = Wire()

        gate.NOTGate(clock, not_clock)
        gate.NOTGate(data, not_data)
        gate.NANDGate2(data, not_clock, wire_1)
        gate.NANDGate2(not_data, not_clock, wire_2)
        gate.NANDGate3(preset_n, wire_1, wire_3, wire_4)
        gate.NANDGate3(wire_4, wire_2, clear_n, wire_3)
        gate.NANDGate2(wire_4, clock, wire_5)
        gate.NANDGate2(wire_3, clock, wire_6)
        gate.NANDGate3(preset_n, wire_5, output_not, output)
        gate.NANDGate3(output, wire_6, clear_n, output_not)


class TFlipFlop:
    """Construct a new positive edge-triggered T flip-flop.

    Args:
        toggle: An object of type Wire. The toggle input to the flip-flop.
        clock: An object of type Wire or Clock. The clock input to the flip-
            flop.
        output: An object of type Wire. The output of the flip-flop. Toggles
            its value on the positive edges of clock if the value of toggle is
            1.
        output_not: An object of type Wire. The complemented form of output.
    """
    def __init__(self, toggle, clock, output, output_not):
        mux_output = Wire()
        enable = Wire()
        enable.value = 1

        signal.Multiplexer2To1(enable, toggle, output_not, output, mux_output)
        DFlipFlop(mux_output, clock, output, output_not)


class TFlipFlopPresetClear:
    """Construct a new positive edge-triggered T flip-flop with preset/clear
        capabilities.

    Args:
        toggle: An object of type Wire. The toggle input to the flip-flop.
        preset_n: An object of type Wire. Presets output to 1 and output_not to
            0 asynchronously if its value is 0.
        clear_n: An object of type Wire. Clears output to 0 and output_not to 1
            asynchronously if its value is 0.
        clock: An object of type Wire or Clock. The clock input to the flip-
            flop.
        output: An object of type Wire. The output of the flip-flop. Toggles
            its value on the positive edges of clock if the value of toggle is
            1.
        output_not: An object of type Wire. The complemented form of output.
    """
    def __init__(self, toggle, preset_n, clear_n, clock, output, output_not):
        mux_output = Wire()
        enable = Wire()
        enable.value = 1

        signal.Multiplexer2To1(enable, toggle, output_not, output, mux_output)
        DFlipFlopPresetClear(
            mux_output,
            preset_n,
            clear_n,
            clock,
            output,
            output_not
        )


class JKFlipFlop:
    """Construct a new positive edge-triggered JK flip-flop.

    Args:
        J: An object of type Wire. The J input to the flip-flop.
        K: An object of type Wire. The K input to the flip-flop.
        clock: An object of type Wire or Clock. The clock input to the flip-
            flop.
        output: An object of type Wire. The output of the flip-flop. On the
            positive edges of clock, takes on the value of 1 if the value of J
            is 1, takes on the value of 0 if the value of K is 1, and toggles
            its value if both J and K have value 1.
        output_not: An object of type Wire. The complemented form of output.
    """
    def __init__(self, j, k, clock, output, output_not):
        and_1 = Wire()
        and_2 = Wire()
        or_1 = Wire()
        not_1 = Wire()

        gate.NOTGate(k, not_1)
        gate.ANDGate2(j, output_not, and_1)
        gate.ANDGate2(not_1, output, and_2)
        gate.ORGate2(and_1, and_2, or_1)
        DFlipFlop(or_1, clock, output, output_not)


class JKFlipFlopPresetClear:
    """Construct a new positive edge-triggered JK flip-flop with preset/clear
        capabilities.

    Args:
        J: An object of type Wire. The J input to the flip-flop.
        K: An object of type Wire. The K input to the flip-flop.
        preset_n: An object of type Wire. Presets output to 1 and output_not to
            0 asynchronously if its value is 0.
        clear_n: An object of type Wire. Clears output to 0 and output_not to 1
            asynchronously if its value is 0.
        clock: An object of type Wire or Clock. The clock input to the flip-
            flop.
        output: An object of type Wire. The output of the flip-flop. On the
            positive edges of clock, takes on the value of 1 if the value of J
            is 1, takes on the value of 0 if the value of K is 1, and toggles
            its value if both J and K have value 1.
        output_not: An object of type Wire. The complemented form of output.
    """
    def __init__(self, j, k, preset_n, clear_n, clock, output, output_not):
        and_1 = Wire()
        and_2 = Wire()
        or_1 = Wire()
        not_1 = Wire()

        gate.NOTGate(k, not_1)
        gate.ANDGate2(j, output_not, and_1)
        gate.ANDGate2(not_1, output, and_2)
        gate.ORGate2(and_1, and_2, or_1)
        DFlipFlopPresetClear(
            or_1,
            preset_n,
            clear_n,
            clock,
            output,
            output_not
        )
