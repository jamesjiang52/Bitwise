"""
The following classes are defined:
    TristateBuffer
"""


class TristateBuffer:
    """Initialize a new tri-state buffer.

    Args:
        enable: An object of type Wire.
        input: An object of type Wire.
        output: An object of type Wire. Takes on the value of input if enable
            has value 1. Otherwise, value is independent of input.
    """
    def __init__(self, enable, input, output):
        self.input = input
        self.enable = enable
        self.output = output

        self.input._bind_to(self._update_input)
        self.enable._bind_to(self._update_enable)

        if enable.value == 1:
            self.output.value = self.input.value
        else:
            pass

    def _update_input(self, value):
        if self.enable.value == 1:
            self.output.value = value
        else:
            pass

    def _update_enable(self, value):
        if value == 1:
            self.output.value = self.input.value
        else:
            pass

    def __str__(self):
        str_ = ""
        str_ += "enable: " + str(self.enable.value) + "\n"
        str_ += "input: " + str(self.input.value) + "\n"
        str_ += "output: " + str(self.output.value)
        return str_

    def __call__(self, *, enable=None, input=None, output=None):
        if enable is not None:
            self.enable.value = enable
        if input is not None:
            self.input.value = input
        if output is not None:
            self.output.value = output
