import AND
import NOT


class NAND:
    """
    This class simulates a NAND gate. By definition, the output of a NAND gate
    is 1 if at least one of its inputs are 0. Otherwise, the output is 0. It is
    constructed simply by inverting the output of an AND gate.
    """
    def __init__(self, *_inputs):
        self._inputs = _inputs

    def get_inputs(self):
        return self._inputs

    def set_inputs(self, *_inputs):
        self._inputs = _inputs

    def get_output(self):
        AND_1 = AND.AND(*self._inputs)
        AND_1_output = AND_1.get_output()

        NOT_1 = NOT.NOT(AND_1_output)
        NOT_1_output = NOT_1.get_output()

        return NOT_1_output
