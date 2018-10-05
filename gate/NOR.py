"""
This module defines classes that simulate NOR gates. By definition, the output
of a NOR gate is 1 if and only if all of its inputs are 0. Otherwise, the
output is 0. It is constructed simply by inverting the output of an OR gate.

The following classes are defined:
    NORGate2
    NORGate3
    NOTGate4
"""

import sys
sys.path.insert(0, "../")
from wire.WIRE import Wire
import OR
import NOT


class NORGate2:
    """
    This NOR gate has two inputs and a single output:
                     ________
        input_1 ----|        |---- output
        input_2 ----|________|

    """
    def __init__(self, input_1, input_2, output):
        wire_1 = Wire(0)
        OR.ORGate2(input_1, input_2, wire_1)
        NOT.NOTGate(wire_1, output)


class NORGate3:
    """
    This NOR gate has three inputs and a single output:
                     ________
        input_1 ----|        |---- output
        input_2 ----|        |
        input_3 ----|________|

    """
    def __init__(self, input_1, input_2, input_3, output):
        wire_1 = Wire(0)
        OR.ORGate3(input_1, input_2, input_3, wire_1)
        NOT.NOTGate(wire_1, output)


class NORGate4:
    """
    This NOR gate has four inputs and a single output:
                     ________
        input_1 ----|        |---- output
        input_2 ----|        |
        input_3 ----|        |
        input_4 ----|________|

    """
    def __init__(self, input_1, input_2, input_3, input_4, output):
        wire_1 = Wire(0)
        OR.ORGate4(input_1, input_2, input_3, input_4, wire_1)
        NOT.NOTGate(wire_1, output)
