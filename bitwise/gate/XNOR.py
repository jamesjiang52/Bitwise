"""
This module defines classes that simulate XNOR gates. By definition, the output
of an XNOR gate is 1 if the inputs are the same (i.e. both 0 or both 1), and 0
if the inputs are different. In other words, the output is 0 if one of the
inputs is 1, but not both. We will only allow two-input XOR gates.

The following classes are defined:
    XNORGate2
"""

from .. import wire
from . import AND
from . import OR
from . import NOT

Wire = wire.Wire


class XNORGate2:
    """
    This XNOR gate has two inputs and a single output:
                     ________
        input_1 ----|        |---- output
        input_2 ----|________|

    """
    def __init__(self, input_1, input_2, output):
        wire_1 = Wire()
        wire_2 = Wire()
        wire_3 = Wire()
        wire_4 = Wire()

        NOT.NOTGate(input_1, wire_1)
        NOT.NOTGate(input_2, wire_2)
        AND.ANDGate2(input_1, input_2, wire_3)
        AND.ANDGate2(wire_1, wire_2, wire_4)
        OR.ORGate2(wire_3, wire_4, output)