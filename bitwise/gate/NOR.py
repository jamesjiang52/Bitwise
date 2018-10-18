"""
The following classes are defined:
    NORGate2
    NORGate3
    NORGate4
"""

from .. import wire
from . import OR
from . import NOT

Wire = wire.Wire


class NORGate2:
    """Construct a new two-input NOR gate.

    Args:
        input_1: An object of type Wire. The first input to the NOR gate.
        input_2: An object of type Wire. The second input to the NOR gate.
        output: An object of type Wire. The output of the NOR gate.
    """
    def __init__(self, input_1, input_2, output):
        wire_1 = Wire()
        OR.ORGate2(input_1, input_2, wire_1)
        NOT.NOTGate(wire_1, output)


class NORGate3:
    """Construct a new three-input NOR gate.

    Args:
        input_1: An object of type Wire. The first input to the NOR gate.
        input_2: An object of type Wire. The second input to the NOR gate.
        input_3: An object of type Wire. The third input to the NOR gate.
        output: An object of type Wire. The output of the NOR gate.
    """
    def __init__(self, input_1, input_2, input_3, output):
        wire_1 = Wire()
        OR.ORGate3(input_1, input_2, input_3, wire_1)
        NOT.NOTGate(wire_1, output)


class NORGate4:
    """Construct a new four-input NOR gate.

    Args:
        input_1: An object of type Wire. The first input to the NOR gate.
        input_2: An object of type Wire. The second input to the NOR gate.
        input_3: An object of type Wire. The third input to the NOR gate.
        input_4: An object of type Wire. The fourth input to the NOR gate.
        output: An object of type Wire. The output of the NOR gate.
    """
    def __init__(self, input_1, input_2, input_3, input_4, output):
        wire_1 = Wire()
        OR.ORGate4(input_1, input_2, input_3, input_4, wire_1)
        NOT.NOTGate(wire_1, output)
