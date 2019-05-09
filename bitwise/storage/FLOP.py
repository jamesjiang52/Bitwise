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
        set: An object of type Wire. The set input to the latch.
        reset: An object of type Wire. The reset input to the latch.
        output: An object of type Wire. The output of the latch. Takes on the
            value of 1 if the value of set is 1 and the value of 0 if the value
            of reset is 1.
        output_not: An object of type Wire. The complemented form of output.
    """
    def __init__(self, set, reset, output, output_not):
        gate.NORGate2(set, output, output_not)
        gate.NORGate2(reset, output_not, output)

        self.set = set
        self.reset = reset
        self.output = output
        self.output_not = output_not

    def __str__(self):
        str_ = ""
        str_ += "set: " + str(self.set.value) + "\n"
        str_ += "reset: " + str(self.reset.value) + "\n"
        str_ += "output: " + str(self.output.value) + "\n"
        str_ += "output_not: " + str(self.output_not.value)
        return str_

    def __call__(
        self, *,
        set=None,
        reset=None,
        output=None,
        output_not=None
    ):
        if set is not None:
            self.set.value = set
        if reset is not None:
            self.reset.value = reset
        if output is not None:
            self.output.value = output
        if output_not is not None:
            self.output_not.value = output_not


class GatedSRLatch:
    """Construct a new gated SR latch.

    Args:
        set: An object of type Wire. The set input to the latch.
        reset: An object of type Wire. The reset input to the latch.
        clock: An object of type Wire or Clock. The clock input to the latch.
        output: An object of type Wire. The output of the latch. When the value
            of clock is 1, takes on the value of 1 if the value of set is 1 and
            the value of 0 if the value of reset is 1.
        output_not: An object of type Wire. The complemented form of output.
    """
    def __init__(self, set, reset, clock, output, output_not):
        wire_1 = Wire()
        wire_2 = Wire()

        gate.ANDGate2(clock, set, wire_1)
        gate.ANDGate2(clock, reset, wire_2)
        SRLatch(wire_1, wire_2, output, output_not)

        self.set = set
        self.reset = reset
        self.clock = clock
        self.output = output
        self.output_not = output_not

    def __str__(self):
        str_ = ""
        str_ += "set: " + str(self.set.value) + "\n"
        str_ += "reset: " + str(self.reset.value) + "\n"
        str_ += "clock: " + str(self.clock.value) + "\n"
        str_ += "output: " + str(self.output.value) + "\n"
        str_ += "output_not: " + str(self.output_not.value)
        return str_

    def __call__(
        self, *,
        set=None,
        reset=None,
        clock=None,
        output=None,
        output_not=None
    ):
        if set is not None:
            self.set.value = set
        if reset is not None:
            self.reset.value = reset
        if clock is not None:
            self.clock.value = clock
        if output is not None:
            self.output.value = output
        if output_not is not None:
            self.output_not.value = output_not


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

        self.data = data
        self.clock = clock
        self.output = output
        self.output_not = output_not

    def __str__(self):
        str_ = ""
        str_ += "data: " + str(self.data.value) + "\n"
        str_ += "clock: " + str(self.clock.value) + "\n"
        str_ += "output: " + str(self.output.value) + "\n"
        str_ += "output_not: " + str(self.output_not.value)
        return str_

    def __call__(
        self, *,
        data=None,
        clock=None,
        output=None,
        output_not=None
    ):
        if data is not None:
            self.data.value = data
        if clock is not None:
            self.clock.value = clock
        if output is not None:
            self.output.value = output
        if output_not is not None:
            self.output_not.value = output_not


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

        self.data = data
        self.clock = clock
        self.output = output
        self.output_not = output_not

    def __str__(self):
        str_ = ""
        str_ += "data: " + str(self.data.value) + "\n"
        str_ += "clock: " + str(self.clock.value) + "\n"
        str_ += "output: " + str(self.output.value) + "\n"
        str_ += "output_not: " + str(self.output_not.value)
        return str_

    def __call__(
        self, *,
        data=None,
        clock=None,
        output=None,
        output_not=None
    ):
        if data is not None:
            self.data.value = data
        if clock is not None:
            self.clock.value = clock
        if output is not None:
            self.output.value = output
        if output_not is not None:
            self.output_not.value = output_not


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

        self.data = data
        self.preset_n = preset_n
        self.clear_n = clear_n
        self.clock = clock
        self.output = output
        self.output_not = output_not

    def __str__(self):
        str_ = ""
        str_ += "data: " + str(self.data.value) + "\n"
        str_ += "preset_n: " + str(self.preset_n.value) + "\n"
        str_ += "clear_n: " + str(self.clear_n.value) + "\n"
        str_ += "clock: " + str(self.clock.value) + "\n"
        str_ += "output: " + str(self.output.value) + "\n"
        str_ += "output_not: " + str(self.output_not.value)
        return str_

    def __call__(
        self, *,
        data=None,
        preset_n=None,
        clear_n=None,
        clock=None,
        output=None,
        output_not=None
    ):
        if data is not None:
            self.data.value = data
        if preset_n is not None:
            self.preset_n.value = preset_n
        if clear_n is not None:
            self.clear_n.value = clear_n
        if clock is not None:
            self.clock.value = clock
        if output is not None:
            self.output.value = output
        if output_not is not None:
            self.output_not.value = output_not


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

        self.toggle = toggle
        self.clock = clock
        self.output = output
        self.output_not = output_not

    def __str__(self):
        str_ = ""
        str_ += "toggle: " + str(self.toggle.value) + "\n"
        str_ += "clock: " + str(self.clock.value) + "\n"
        str_ += "output: " + str(self.output.value) + "\n"
        str_ += "output_not: " + str(self.output_not.value)
        return str_

    def __call__(
        self, *,
        toggle=None,
        clock=None,
        output=None,
        output_not=None
    ):
        if toggle is not None:
            self.toggle.value = toggle
        if clock is not None:
            self.clock.value = clock
        if output is not None:
            self.output.value = output
        if output_not is not None:
            self.output_not.value = output_not


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

        self.toggle = toggle
        self.preset_n = preset_n
        self.clear_n = clear_n
        self.clock = clock
        self.output = output
        self.output_not = output_not

    def __str__(self):
        str_ = ""
        str_ += "toggle: " + str(self.toggle.value) + "\n"
        str_ += "preset_n: " + str(self.preset_n.value) + "\n"
        str_ += "clear_n: " + str(self.clear_n.value) + "\n"
        str_ += "clock: " + str(self.clock.value) + "\n"
        str_ += "output: " + str(self.output.value) + "\n"
        str_ += "output_not: " + str(self.output_not.value)
        return str_

    def __call__(
        self, *,
        toggle=None,
        preset_n=None,
        clear_n=None,
        clock=None,
        output=None,
        output_not=None
    ):
        if toggle is not None:
            self.toggle.value = toggle
        if preset_n is not None:
            self.preset_n.value = preset_n
        if clear_n is not None:
            self.clear_n.value = clear_n
        if clock is not None:
            self.clock.value = clock
        if output is not None:
            self.output.value = output
        if output_not is not None:
            self.output_not.value = output_not


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

        self.J = j
        self.K = k
        self.clock = clock
        self.output = output
        self.output_not = output_not

    def __str__(self):
        str_ = ""
        str_ += "J: " + str(self.J.value) + "\n"
        str_ += "K: " + str(self.K.value) + "\n"
        str_ += "clock: " + str(self.clock.value) + "\n"
        str_ += "output: " + str(self.output.value) + "\n"
        str_ += "output_not: " + str(self.output_not.value)
        return str_

    def __call__(
        self, *,
        J=None,
        K=None,
        clock=None,
        output=None,
        output_not=None
    ):
        if J is not None:
            self.J.value = J
        if K is not None:
            self.K.value = K
        if clock is not None:
            self.clock.value = clock
        if output is not None:
            self.output.value = output
        if output_not is not None:
            self.output_not.value = output_not


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

        self.J = j
        self.K = k
        self.preset_n = preset_n
        self.clear_n = clear_n
        self.clock = clock
        self.output = output
        self.output_not = output_not

    def __str__(self):
        str_ = ""
        str_ += "J: " + str(self.J.value) + "\n"
        str_ += "K: " + str(self.K.value) + "\n"
        str_ += "preset_n: " + str(self.preset_n.value) + "\n"
        str_ += "clear_n: " + str(self.clear_n.value) + "\n"
        str_ += "clock: " + str(self.clock.value) + "\n"
        str_ += "output: " + str(self.output.value) + "\n"
        str_ += "output_not: " + str(self.output_not.value)
        return str_

    def __call__(
        self, *,
        J=None,
        K=None,
        preset_n=None,
        clear_n=None,
        clock=None,
        output=None,
        output_not=None
    ):
        if J is not None:
            self.J.value = J
        if K is not None:
            self.K.value = K
        if preset_n is not None:
            self.preset_n.value = preset_n
        if clear_n is not None:
            self.clear_n.value = clear_n
        if clock is not None:
            self.clock.value = clock
        if output is not None:
            self.output.value = output
        if output_not is not None:
            self.output_not.value = output_not
