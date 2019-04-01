"""
The following classes are defined:
    XNORGate2
"""

from .. import wire
from . import AND
from . import OR
from . import NOT

Wire = wire.Wire


class XNORGate2:
    """Construct a new two-input XNOR gate.

    Args:
        input_1: An object of type Wire. The first input to the XNOR gate.
        input_2: An object of type Wire. The second input to the XNOR gate.
        output: An object of type Wire. The output of the XNOR gate.
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

        self.input_1 = input_1
        self.input_2 = input_2
        self.output = output

    def __str__(self):
        str_ = ""
        str_ += "input_1: " + str(self.input_1.value) + "\n"
        str_ += "input_2: " + str(self.input_2.value) + "\n"
        str_ += "output: " + str(self.output.value)
        return str_
