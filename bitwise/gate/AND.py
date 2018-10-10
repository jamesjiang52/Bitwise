"""
This module defines classes that simulate AND gates. By definition, the output
of an AND gate is 1 if and only if all of its inputs are 1. Otherwise, the
output is 0.

The following classes are defined:
    ANDGate2
    ANDGate3
    ANDGate4
"""

from .. import wire

Wire = wire.Wire


class ANDGate2:
    """
    This AND gate has two inputs and a single output:
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

        if ((self.input_1.value == 1) and
                (self.input_2.value == 1)):
            self.output.value = 1
        else:
            self.output.value = 0

    def update_input_1(self, value):
        if ((value == 1) and (self.input_2.value == 1)):
            self.output.value = 1
        else:
            self.output.value = 0

    def update_input_2(self, value):
        if ((self.input_1.value == 1) and (value == 1)):
            self.output.value = 1
        else:
            self.output.value = 0


class ANDGate3:
    """
    This AND gate has three inputs and a single output:
                     ________
        input_1 ----|        |---- output
        input_2 ----|        |
        input_3 ----|________|

    """
    def __init__(self, input_1, input_2, input_3, output):
        wire_1 = Wire()
        ANDGate2(input_1, input_2, wire_1)
        ANDGate2(input_3, wire_1, output)


class ANDGate4:
    """
    This AND gate has four inputs and a single output:
                     ________
        input_1 ----|        |---- output
        input_2 ----|        |
        input_3 ----|        |
        input_4 ----|________|

    """
    def __init__(self, input_1, input_2, input_3, input_4, output):
        wire_1 = Wire()
        wire_2 = Wire()
        ANDGate2(input_1, input_2, wire_1)
        ANDGate2(input_3, input_4, wire_2)
        ANDGate2(wire_1, wire_2, output)
