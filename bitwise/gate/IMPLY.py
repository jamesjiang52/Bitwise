"""
The following classes are defined:
    IMPLYGate2
"""

from .. import wire
from . import OR
from . import NOT

Wire = wire.Wire


class IMPLYGate:
    """Construct a new IMPLY gate.

    Args:
        input_1: An object of type Wire. The first input to the IMPLY gate.
        input_2: An object of type Wire. The second input to the IMPLY gate.
        output: An object of type Wire. The output of the IMPLY gate.
    """
    def __init__(self, input_1, input_2, output):
        wire_1 = Wire()
        NOT.NOTGate(input_1, wire_1)
        OR.ORGate2(wire_1, input_2, output)
