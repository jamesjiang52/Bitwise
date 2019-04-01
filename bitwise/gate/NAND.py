"""
The following classes are defined:
    NANDGate2
    NANDGate3
    NANDGate4
"""

from .. import wire
from . import AND
from . import NOT

Wire = wire.Wire


class NANDGate2:
    """Construct a new two-input NAND gate.

    Args:
        input_1: An object of type Wire. The first input to the NAND gate.
        input_2: An object of type Wire. The second input to the NAND gate.
        output: An object of type Wire. The output of the NAND gate.
    """
    def __init__(self, input_1, input_2, output):
        wire_1 = Wire()
        AND.ANDGate2(input_1, input_2, wire_1)
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


class NANDGate3:
    """Construct a new three-input NAND gate.

    Args:
        input_1: An object of type Wire. The first input to the NAND gate.
        input_2: An object of type Wire. The second input to the NAND gate.
        input_3: An object of type Wire. The third input to the NAND gate.
        output: An object of type Wire. The output of the NAND gate.
    """
    def __init__(self, input_1, input_2, input_3, output):
        wire_1 = Wire()
        AND.ANDGate3(input_1, input_2, input_3, wire_1)
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


class NANDGate4:
    """Construct a new four-input NAND gate.

    Args:
        input_1: An object of type Wire. The first input to the NAND gate.
        input_2: An object of type Wire. The second input to the NAND gate.
        input_3: An object of type Wire. The third input to the NAND gate.
        input_4: An object of type Wire. The fourth input to the NAND gate.
        output: An object of type Wire. The output of the NAND gate.
    """
    def __init__(self, input_1, input_2, input_3, input_4, output):
        wire_1 = Wire()
        AND.ANDGate4(input_1, input_2, input_3, input_4, wire_1)
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
