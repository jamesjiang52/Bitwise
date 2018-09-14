import OR
import NOT


class NOR:
    """
    This class simulates a NOR gate. By definition, the output of a NOR gate is
    1 if and only if all of its inputs are 0. Otherwise, the output is 0. It is
    constructed simply by inverting the output of an OR gate.
    """
    def __init__(self, *_inputs):
        self._inputs = _inputs

    def get_inputs(self):
        return self._inputs

    def set_inputs(self, *_inputs):
        self._inputs = _inputs

    def get_output(self):
        OR_1 = OR.OR(*self._inputs)
        OR_1_output = OR_1.get_output()

        NOT_1 = NOT.NOT(OR_1_output)
        NOT_1_output = NOT_1.get_output()

        return NOT_1_output
