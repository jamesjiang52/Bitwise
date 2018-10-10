"""

"""

from .. import wire
from .. import gate

Wire = wire.Wire


class SRLatch:
    """

    """
    def __init__(self, set_, reset, output, output_not):
        gate.NORGate2(set_, output, output_not)
        gate.NORGate2(reset, output_not, output)


class GatedSRLatch:
    """

    """
    def __init__(self, clock, set_, reset, output, output_not):
        wire_1 = Wire()
        wire_2 = Wire()

        gate.ANDGate2(clock, set_, wire_1)
        gate.ANDGate2(clock, reset, wire_2)
        SRLatch(wire_1, wire_2, output, output_not)


class GatedDLatch:
    """

    """
    def __init__(self, clock, data, output, output_not):
        wire_1 = Wire()

        gate.NOTGate(data, wire_1)
        GatedSRLatch(clock, data, wire_1, output, output_not)


class DFlipFlop:
    """

    """
    def __init__(self, clock, data, output, output_not):
        pass
