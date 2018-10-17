class NOTGate:
    def __init__(self, input_1, output):
        self.input_1 = input_1
        self.output = output

        self.input_1._bind_to(self._update_input_1)

        if self.input_1.value == 1:
            self.output.value = 0
        else:
            self.output.value = 1

    def _update_input_1(self, value):
        if value == 1:
            self.output.value = 0
        else:
            self.output.value = 1
