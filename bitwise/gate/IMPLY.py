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

        self.input_1 = input_1
        self.input_2 = input_2
        self.output = output

    def __str__(self):
        str_ = ""
        str_ += "input_1: " + str(self.input_1.value) + "\n"
        str_ += "input_2: " + str(self.input_2.value) + "\n"
        str_ += "output: " + str(self.output.value)
        return str_

    def __call__(self, *, input_1=None, input_2=None, output=None):
        if input_1 is not None:
            self.input_1.value = input_1
        if input_2 is not None:
            self.input_2.value = input_2
        if output is not None:
            self.output.value = output
