"""
This module defines classes that simulate primitive storage elements, namely
1-bit latches and 1-bit flip-flops. Latches are level-sensitive, changing their
value according to the value of a clock input, while flip-flops are edge-
sensitive, changing their value according to edges of a clock input.

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
    """
    This class simulates an SR latch, which has two inputs and two outputs:
                   ________
          set ----|        |---- output
        reset ----|________|---- output_not

    If set is 1 and reset is 0, output and output_not are 1 and 0,
    respectively. If set is 0 and reset is 1, output and output_not are 0 and
    1, respectively. If both set and reset are 0, output and output_not hold
    their current values. The input where both set and reset are 1 is not used.
    """
    def __init__(self, set_, reset, output, output_not):
        gate.NORGate2(set_, output, output_not)
        gate.NORGate2(reset, output_not, output)


class GatedSRLatch:
    """
    This class simulates a gated SR latch, which has three inputs, including a
    clock, and two outputs:
                   ________
          set ----|        |---- output
        reset ----|        |---- output_not
        clock ----|________|

    If clock is 0, output and output_not hold their current values, regardless
    of the set and reset inputs. If clock is 1 and set and reset are 1 and 0,
    respectively, output and output_not are 1 and 0, respectively. If clock is
    1 and set and reset are 1 and 0, respectively, output and output_not are 0
    and 1, respectively. If clock is 1 and both set and reset are 0, output and
    output_not hold their current values. The input where both set and reset
    are 1 is not used.
    """
    def __init__(self, set_, reset, clock, output, output_not):
        wire_1 = Wire()
        wire_2 = Wire()

        gate.ANDGate2(clock, set_, wire_1)
        gate.ANDGate2(clock, reset, wire_2)
        SRLatch(wire_1, wire_2, output, output_not)


class GatedDLatch:
    """
    This class simulates a gated D latch, which has two inputs, including a
    clock, and two outputs:
                   ________
         data ----|        |---- output
        clock ----|________|---- output_not

    If clock is 0, output and output_not hold their current values, regardless
    of the data input. If clock is 1, output takes on the value of data and
    output_not takes on the opposite value.
    """
    def __init__(self, data, clock, output, output_not):
        wire_1 = Wire()

        gate.NOTGate(data, wire_1)
        GatedSRLatch(data, wire_1, clock, output, output_not)


class DFlipFlop:
    """
    This class simulates a D flip-flop, which as two inputs, including a clock,
    and two outputs:
                   ________
         data ----|        |---- output
        clock ----|________|---- output_not

    On the positive edge of clock (i.e. on the clock transition from a 0 value
    to a 1 value), output takes on the value of data and output_not takes on
    the opposite value. Otherwise, output and output_not hold their current
    values.
    """
    def __init__(self, data, clock, output, output_not):
        q_1 = Wire()
        q_not_1 = Wire()
        not_clock = Wire()

        gate.NOTGate(clock, not_clock)
        GatedDLatch(data, not_clock, q_1, q_not_1)
        GatedDLatch(q_1, clock, output, output_not)


class TFlipFlop:
    """

    """
    def __init__(self, toggle, clock, output, output_not):
        mux_output = Wire()

        signal.Multiplexer2To1(1, toggle, output_not, output, mux_output)
        DFlipFlop(mux_output, clock, output, output_not)


class JKFlipFlop:
    """

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
