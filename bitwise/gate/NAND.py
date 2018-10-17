from .. import wire
from . import AND
from . import NOT

Wire = wire.Wire


class NANDGate2:
    def __init__(self, input_1, input_2, output):
        wire_1 = Wire()
        AND.ANDGate2(input_1, input_2, wire_1)
        NOT.NOTGate(wire_1, output)


class NANDGate3:
    def __init__(self, input_1, input_2, input_3, output):
        wire_1 = Wire()
        AND.ANDGate3(input_1, input_2, input_3, wire_1)
        NOT.NOTGate(wire_1, output)


class NANDGate4:
    def __init__(self, input_1, input_2, input_3, input_4, output):
        wire_1 = Wire()
        AND.ANDGate4(input_1, input_2, input_3, input_4, wire_1)
        NOT.NOTGate(wire_1, output)
