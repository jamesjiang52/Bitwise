"""
The following classes are defined:
    TristateBuffer
"""


class TristateBuffer:
    """Initialize a new tri-state buffer.

    Args:
        input_1: An object of type Wire.
        switch: An object of type Wire.
        output: An object of type Wire. Takes on the value of input_1 if switch
            has value 1. Otherwise, value is independent of input_1.
    """
    def __init__(self, input_1, switch, output):
        self.input_1 = input_1
        self.switch = switch
        self.output = output

        self.input_1._bind_to(self._update_input_1)
        self.switch._bind_to(self._update_switch)

        if switch.value == 1:
            self.output.value = self.input_1.value
        else:
            pass

    def _update_input_1(self, value):
        if self.switch.value == 1:
            self.output.value = value
        else:
            pass

    def _update_switch(self, value):
        if value == 1:
            self.output.value = self.input_1.value
        else:
            pass
