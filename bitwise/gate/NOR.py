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

        self.input_1 = input_1
        self.input_2 = input_2
        self.output = output

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
