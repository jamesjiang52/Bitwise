"""
The following classes are defined:
    NOTGate
"""

from .. import wire

Wire = wire.Wire


class NOTGate:
    """Construct a new NOT gate.

    Args:
        input: An object of type Wire. The input to the NOT gate.
        output: An object of type Wire. The output of the NOT gate.
    """
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
