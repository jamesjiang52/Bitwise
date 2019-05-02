"""
The following classes are defined:
    ORGate2
    ORGate3
    ORGate4
"""

from .. import wire

Wire = wire.Wire


class ORGate2:
    """Construct a new two-input OR gate.

    Args:
        input_1: An object of type Wire. The first input to the OR gate.
        input_2: An object of type Wire. The second input to the OR gate.
        output: An object of type Wire. The output of the OR gate.
    """
    def __init__(self, input_1, input_2, output):
        self.input_1 = input_1
        self.input_2 = input_2
        self.output = output

        self.input_1._bind_to(self._update_input_1)
        self.input_2._bind_to(self._update_input_2)

        if ((self.input_1.value == 1) or
                (self.input_2.value == 1)):
            self.output.value = 1
        else:
            self.output.value = 0

        self.input_1 = input_1
        self.input_2 = input_2
        self.output = output

    def _update_input_1(self, value):
        if ((value == 1) or (self.input_2.value == 1)):
            self.output.value = 1
        else:
            self.output.value = 0

    def _update_input_2(self, value):
        if ((self.input_1.value == 1) or (value == 1)):
            self.output.value = 1
        else:
            self.output.value = 0

    def __str__(self):
        str_ = ""
        str_ += "input_1: " + str(self.input_1.value) + "\n"
        str_ += "input_2: " + str(self.input_2.value) + "\n"
        str_ += "output: " + str(self.output.value)
        return str_

    def __call__(
        self, *,
        input_1=None,
        input_2=None,
        output=None
    ):
        if input_1 is not None:
            self.input_1.value = input_1
        if input_2 is not None:
            self.input_2.value = input_2
        if output is not None:
            self.output.value = output


class ORGate3:
    """Construct a new three-input OR gate.

    Args:
        input_1: An object of type Wire. The first input to the OR gate.
        input_2: An object of type Wire. The second input to the OR gate.
        input_3: An object of type Wire. The third input to the OR gate.
        output: An object of type Wire. The output of the OR gate.
    """
    def __init__(self, input_1, input_2, input_3, output):
        wire_1 = Wire()
        ORGate2(input_1, input_2, wire_1)
        ORGate2(input_3, wire_1, output)

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

    def __call__(
        self, *,
        input_1=None,
        input_2=None,
        input_3=None,
        output=None
    ):
        if input_1 is not None:
            self.input_1.value = input_1
        if input_2 is not None:
            self.input_2.value = input_2
        if input_3 is not None:
            self.input_3.value = input_3
        if output is not None:
            self.output.value = output


class ORGate4:
    """Construct a new four-input OR gate.

    Args:
        input_1: An object of type Wire. The first input to the OR gate.
        input_2: An object of type Wire. The second input to the OR gate.
        input_3: An object of type Wire. The third input to the OR gate.
        input_4: An object of type Wire. The fourth input to the OR gate.
        output: An object of type Wire. The output of the OR gate.
    """
    def __init__(self, input_1, input_2, input_3, input_4, output):
        wire_1 = Wire()
        wire_2 = Wire()
        ORGate2(input_1, input_2, wire_1)
        ORGate2(input_3, input_4, wire_2)
        ORGate2(wire_1, wire_2, output)

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

    def __call__(
        self, *,
        input_1=None,
        input_2=None,
        input_3=None,
        input_4=None,
        output=None
    ):
        if input_1 is not None:
            self.input_1.value = input_1
        if input_2 is not None:
            self.input_2.value = input_2
        if input_3 is not None:
            self.input_3.value = input_3
        if input_4 is not None:
            self.input_4.value = input_4
        if output is not None:
            self.output.value = output
