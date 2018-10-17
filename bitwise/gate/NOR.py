from .. import wire
from . import OR
from . import NOT

Wire = wire.Wire


class NORGate2:
    def __init__(self, input_1, input_2, output):
        wire_1 = Wire()
        OR.ORGate2(input_1, input_2, wire_1)
        NOT.NOTGate(wire_1, output)


class NORGate3:
    def __init__(self, input_1, input_2, input_3, output):
        wire_1 = Wire()
        OR.ORGate3(input_1, input_2, input_3, wire_1)
        NOT.NOTGate(wire_1, output)


class NORGate4:
    def __init__(self, input_1, input_2, input_3, input_4, output):
        wire_1 = Wire()
        OR.ORGate4(input_1, input_2, input_3, input_4, wire_1)
        NOT.NOTGate(wire_1, output)
