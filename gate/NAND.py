"""
This module defines classes that simulate NAND gates. By definition, the output
of a NAND gate is 1 if at least one of its inputs are 0. Otherwise, the output
is 0. It is constructed simply by inverting the output of an AND gate.

The following classes are defined:
    NANDGate2
    NANDGate3
    NANDGate4
"""

import sys
sys.path.insert(0, "../")
from wire.WIRE import Wire
import AND
import NOT


class NANDGate2:
    """
    This NAND gate has two inputs and a single output:
                     ________
        input_1 ----|        |---- output
        input_2 ----|________|

    """
    def __init__(self, input_1, input_2, output):
        wire_1 = Wire(0)
        AND.ANDGate2(input_1, input_2, wire_1)
        NOT.NOTGate(wire_1, output)


class NANDGate3:
    """
    This NAND gate has three inputs and a single output:
                     ________
        input_1 ----|        |---- output
        input_2 ----|        |
        input_3 ----|________|

    """
    def __init__(self, input_1, input_2, input_3, output):
        wire_1 = Wire(0)
        AND.ANDGate3(input_1, input_2, input_3, wire_1)
        NOT.NOTGate(wire_1, output)


class NANDGate4:
    """
    This NAND gate has four inputs and a single output:
                     ________
        input_1 ----|        |---- output
        input_2 ----|        |
        input_3 ----|        |
        input_4 ----|________|

    """
    def __init__(self, input_1, input_2, input_3, input_4, output):
        wire_1 = Wire(0)
        AND.ANDGate4(input_1, input_2, input_3, input_4, wire_1)
        NOT.NOTGate(wire_1, output)
