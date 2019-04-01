"""
The following classes are defined:
    ANDGate2
    ANDGate3
    ANDGate4
"""

from .. import wire

Wire = wire.Wire


class ANDGate2:
    """Construct a new two-input AND gate.

    Args:
        input_1: An object of type Wire. The first input to the AND gate.
        input_2: An object of type Wire. The second input to the AND gate.
        output: An object of type Wire. The output of the AND gate.
    """
    def __init__(self, input_1, input_2, output):
        self.input_1 = input_1
        self.input_2 = input_2
        self.output = output

        self.input_1._bind_to(self._update_input_1)
        self.input_2._bind_to(self._update_input_2)

        if ((self.input_1.value == 1) and
                (self.input_2.value == 1)):
            self.output.value = 1
        else:
            self.output.value = 0

        self.input_1 = input_1
        self.input_2 = input_2
        self.output = output

    def _update_input_1(self, value):
        if ((value == 1) and (self.input_2.value == 1)):
            self.output.value = 1
        else:
            self.output.value = 0

    def _update_input_2(self, value):
        if ((self.input_1.value == 1) and (value == 1)):
            self.output.value = 1
        else:
            self.output.value = 0

    def __str__(self):
        str_ = ""
        str_ += "input_1: " + str(self.input_1.value) + "\n"
        str_ += "input_2: " + str(self.input_2.value) + "\n"
        str_ += "output: " + str(self.output.value)
        return str_


class ANDGate3:
    """Construct a new three-input AND gate.

    Args:
        input_1: An object of type Wire. The first input to the AND gate.
        input_2: An object of type Wire. The second input to the AND gate.
        input_3: An object of type Wire. The third input to the AND gate.
        output: An object of type Wire. The output of the AND gate.
    """
    def __init__(self, input_1, input_2, input_3, output):
        wire_1 = Wire()
        ANDGate2(input_1, input_2, wire_1)
        ANDGate2(input_3, wire_1, output)

        self.input_1 = input_1
        self.input_2 = input_2
        self.input_3 = input_3
        self.output = output

    def __str__(self):
        str_ = ""
        str_ += "input_1: " + str(self.input_1.value) + "\n"
        str_ += "input_2: " + str(self.input_2.value) + "\n"
        str_ += "input_3: " + str(self.input_3.value) + "\n"
        str_ += "output: " + str(self.output.value)
        return str_


class ANDGate4:
    """Construct a new four-input AND gate.

    Args:
        input_1: An object of type Wire. The first input to the AND gate.
        input_2: An object of type Wire. The second input to the AND gate.
        input_3: An object of type Wire. The third input to the AND gate.
        input_4: An object of type Wire. The fourth input to the AND gate.
        output: An object of type Wire. The output of the AND gate.
    """
    def __init__(self, input_1, input_2, input_3, input_4, output):
        wire_1 = Wire()
        wire_2 = Wire()
        ANDGate2(input_1, input_2, wire_1)
        ANDGate2(input_3, input_4, wire_2)
        ANDGate2(wire_1, wire_2, output)

        self.input_1 = input_1
        self.input_2 = input_2
        self.input_3 = input_3
        self.input_4 = input_4
        self.output = output

    def __str__(self):
        str_ = ""
        str_ += "input_1: " + str(self.input_1.value) + "\n"
        str_ += "input_2: " + str(self.input_2.value) + "\n"
        str_ += "input_3: " + str(self.input_3.value) + "\n"
        str_ += "input_4: " + str(self.input_4.value) + "\n"
        str_ += "output: " + str(self.output.value)
        return str_
