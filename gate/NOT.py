class NOTGate:
    """
    This class simulates a NOT gate, otherwise known as an inverter. The output
    is 1 if the input is 0, and 0 if the input is 1.

    This NOT gate has a single input and a single output:
                   ________
        input ----|________|---- output

    """
    def __init__(self, input_1, output):
        self.input_1 = input_1
        self.output = output

        self.input_1.bind_to(self.update_input_1)

        if self.input_1.value == 1:
            self.output.value = 0
        else:
            self.output.value = 1

    def update_input_1(self, value):
        if value == 1:
            self.output.value = 0
        else:
            self.output.value = 1
