from .. import wire
from . import AND
from . import OR
from . import NOT

Wire = wire.Wire


class XORGate2:
    def __init__(self, input_1, input_2, output):
        wire_1 = Wire()
        wire_2 = Wire()
        wire_3 = Wire()
        wire_4 = Wire()

        NOT.NOTGate(input_1, wire_1)
        NOT.NOTGate(input_2, wire_2)
        AND.ANDGate2(input_1, wire_2, wire_3)
        AND.ANDGate2(input_2, wire_1, wire_4)
        OR.ORGate2(wire_3, wire_4, output)
