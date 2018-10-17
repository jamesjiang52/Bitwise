from .. import wire

Wire = wire.Wire


class ANDGate2:
    def __init__(self, input_1, input_2, output):
        self.input_1 = input_1
        self.input_2 = input_2
        self.output = output

        self.input_1._bind_to(self._update_input_1)
        self.input_2._bind_to(self._update_input_2)

        if ((self.input_1.value == 1) and
                (self.input_2.value == 1)):
            self.output.value = 1
        else:
            self.output.value = 0

    def _update_input_1(self, value):
        if ((value == 1) and (self.input_2.value == 1)):
            self.output.value = 1
        else:
            self.output.value = 0

    def _update_input_2(self, value):
        if ((self.input_1.value == 1) and (value == 1)):
            self.output.value = 1
        else:
            self.output.value = 0


class ANDGate3:
    def __init__(self, input_1, input_2, input_3, output):
        wire_1 = Wire()
        ANDGate2(input_1, input_2, wire_1)
        ANDGate2(input_3, wire_1, output)


class ANDGate4:
    def __init__(self, input_1, input_2, input_3, input_4, output):
        wire_1 = Wire()
        wire_2 = Wire()
        ANDGate2(input_1, input_2, wire_1)
        ANDGate2(input_3, input_4, wire_2)
        ANDGate2(wire_1, wire_2, output)
