"""
This module defines classes that simulate majority gates. A majority gate
receives a number of inputs and outputs the value 0 if the majority of inputs
is 0 and the value 1 if the majority of inputs is 1. If there is the same
number of 0's and 1's in the input, the majority gate outputs the value 0.

The following classes are defined:
    MajorityOf4
"""

from .. import wire
from .. import gate

Wire = wire.Wire


class MajorityOf4:
    """
    This majority gate has four inputs and a single output:
                     ________
        input_1 ----|        |---- output
        input_2 ----|        |
        input_3 ----|        |
        input_4 ----|________|

    """
    def __init__(self, input_1, input_2, input_3, input_4, output):
        wire_1 = Wire()
        wire_2 = Wire()
        wire_3 = Wire()
        wire_4 = Wire()

        gate.ANDGate3(input_1, input_2, input_3, wire_1)
        gate.ANDGate3(input_1, input_2, input_4, wire_2)
        gate.ANDGate3(input_1, input_3, input_4, wire_3)
        gate.ANDGate3(input_2, input_3, input_4, wire_4)

        gate.ORGate4(wire_1, wire_2, wire_3, wire_4, output)
