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
    def __init__(self, input_1, output):
        self.input_1 = input_1
        self.output = output

        self.input_1._bind_to(self._update_input_1)

        if self.input_1.value == 1:
            self.output.value = 1
        else:
            self.output.value = 0

        self.input = input_1
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
