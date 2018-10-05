from AND import AND
from OR import OR
from NOT import NOT


class XOR:
    def __init__(self, input_1_wire, input_2_wire, output_wire):
        if (input_1_wire.get_value() == 1) and (input_2_wire.get_value() == 1):
            output_wire.set_value(0)
        if (input_1_wire.get_value() == 0) and (input_2_wire.get_value() == 1):
            output_wire.set_value(1)
        if (input_1_wire.get_value() == 1) and (input_2_wire.get_value() == 0):
            output_wire.set_value(1)
        else:
            output_wire.set_value(0)
