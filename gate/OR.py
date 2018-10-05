"""
This module defines classes that simulate OR gates. By definition, the output
of an OR gate is 1 if at least one of its inputs are 1. Otherwise, the output
is 0.

The following classes are defined:
    ORGate2
    ORGate3
    ORGate4
"""

import sys
sys.path.insert(0, "../")
from wire.WIRE import Wire


class ORGate2:
    """
    This OR gate has two inputs and a single output:
                     ________
        input_1 ----|        |---- output
        input_2 ----|________|

    """
    def __init__(self, input_1, input_2, output):
        self.input_1 = input_1
        self.input_2 = input_2
        self.output = output

        self.input_1.bind_to(self.update_input_1)
        self.input_2.bind_to(self.update_input_2)

        if ((self.input_1.value == 1) or
                (self.input_2.value == 1)):
            self.output.value = 1
        else:
            self.output.value = 0

    def update_input_1(self, value):
        if ((value == 1) or (self.input_2.value == 1)):
            self.output.value = 1
        else:
            self.output.value = 0

    def update_input_2(self, value):
        if ((self.input_1.value == 1) or (value == 1)):
            self.output.value = 1
        else:
            self.output.value = 0


class ORGate3:
    """
    This OR gate has three inputs and a single output:
                     ________
        input_1 ----|        |---- output
        input_2 ----|        |
        input_3 ----|________|

    """
    def __init__(self, input_1, input_2, input_3, output):
        wire_1 = Wire(0)
        ORGate2(input_1, input_2, wire_1)
        ORGate2(input_3, wire_1, output)


class ORGate4:
    """
    This OR gate has four inputs and a single output:
                     ________
        input_1 ----|        |---- output
        input_2 ----|        |
        input_3 ----|        |
        input_4 ----|________|

    """
    def __init__(self, input_1, input_2, input_3, input_4, output):
        wire_1 = Wire(0)
        wire_2 = Wire(0)
        ORGate2(input_1, input_2, wire_1)
        ORGate2(input_3, input_4, wire_2)
        ORGate2(wire_1, wire_2, output)
