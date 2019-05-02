"""
The following classes are defined:
    Buffer
"""

from .. import wire

Wire = wire.Wire


class Buffer:
    """Construct a new Buffer.

    Args:
        input: An object of type Wire. The input to the buffer.
        output: An object of type Wire. The output of the buffer.
    """
    def __init__(self, input, output):
        self.input = input
        self.output = output

        self.input._bind_to(self._update_input_1)

        if self.input.value == 1:
            self.output.value = 1
        else:
            self.output.value = 0

        self.input = input
        self.output = output

    def _update_input_1(self, value):
        if value == 1:
            self.output.value = 1
        else:
            self.output.value = 0

    def __str__(self):
        str_ = ""
        str_ += "input: " + str(self.input.value) + "\n"
        str_ += "output: " + str(self.output.value)
        return str_

    def __call__(self, *, input=None, output=None):
        if input is not None:
            self.input.value = input
        if output is not None:
            self.output.value = output
