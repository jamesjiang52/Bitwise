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
