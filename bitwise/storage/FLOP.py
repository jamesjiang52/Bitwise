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
    This class simulates a D flip-flop, which has two inputs, including a
    clock, and two outputs:
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


class DFlipFlopPresetClear:
    """
    This class simulates a D flip-flop, with additional inputs for presetting
    and clearing the flip_flop. It has four inputs, including a clock, and two
    outputs:
                      ________
            data ----|        |---- output
        preset_n ----|        |---- output_not
         clear_n ----|        |
           clock ----|________|

    On the positive edge of clock (i.e. on the clock transition from a 0 value
    to a 1 value), output takes on the value of data and output_not takes on
    the opposite value. Otherwise, output and output_not hold their current
    values.

    Inputs preset_n and clear_n are, respectively, an active low
    asynchronous preset (setting output to 1 and output_not to 0) and an active
    low asynchronous clear (setting output to 0 and output_not to 1).
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
    """
    This class simulates a T flip-flop, which has two inputs, including a
    clock, and two outputs:
                    ________
        toggle ----|        |---- output
         clock ----|________|---- output_not

    On the positive edge of clock (i.e. on the clock transition from a 0 value
    to a 1 value), if toggle has the value 1, both output and output_not toggle
    their current values. If toggle has the value 0, both output and output_not
    are unchanged.
    """
    def __init__(self, toggle, clock, output, output_not):
        mux_output = Wire()
        enable = Wire()
        enable.value = 1

        signal.Multiplexer2To1(enable, toggle, output_not, output, mux_output)
        DFlipFlop(mux_output, clock, output, output_not)


class TFlipFlopPresetClear:
    """
    This class simulates a T flip-flop, with additional inputs for presetting
    and clearing the flip_flop. It has four inputs, including a clock, and two
    outputs:
                      ________
          toggle ----|        |---- output
        preset_n ----|        |---- output_not
         clear_n ----|        |
           clock ----|________|

    On the positive edge of clock (i.e. on the clock transition from a 0 value
    to a 1 value), if toggle has the value 1, both output and output_not toggle
    their current values. If toggle has the value 0, both output and output_not
    are unchanged.

    Inputs preset_n and clear_n are, respectively, an active low
    asynchronous preset (setting output to 1 and output_not to 0) and an active
    low asynchronous clear (setting output to 0 and output_not to 1).
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
    """
    This class simulates a JK flip-flop, which has three inputs, including a
    clock, and two outputs:
                   ________
            J ----|        |---- output
            K ----|        |---- output_not
        clock ----|________|

    On the positive edge of clock (i.e. on the clock transition from a 0 value
    to a 1 value), if J is 1 and K is 0, output and output_not are 1 and 0,
    respectively. If J is 0 and K is 1, output and output_not are 0 and 1,
    respectively. If J and K are both 0, both output and output_not hold their
    current values. If J and K are both 1, both output and output_not toggle
    their current values.
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
    """
    This class simulates a JK flip-flop, with additional inputs for presetting
    and clearing the flip_flop. It has five inputs, including a clock, and two
    outputs:
                      ________
               J ----|        |---- output
               K ----|        |---- output_not
        preset_n ----|        |
         clear_n ----|        |
           clock ----|________|

    On the positive edge of clock (i.e. on the clock transition from a 0 value
    to a 1 value), if J is 1 and K is 0, output and output_not are 1 and 0,
    respectively. If J is 0 and K is 1, output and output_not are 0 and 1,
    respectively. If J and K are both 0, both output and output_not hold their
    current values. If J and K are both 1, both output and output_not toggle
    their current values.

    Inputs preset_n and clear_n are, respectively, an active low
    asynchronous preset (setting output to 1 and output_not to 0) and an active
    low asynchronous clear (setting output to 0 and output_not to 1).
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
