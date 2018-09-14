import AND
import OR
import NOT


class XOR:
    """
    This class simulates an XOR gate. By definition, the output of an XOR gate
    is 1 if the inputs are different, and 0 if the inputs are the same (i.e.
    both 0 or both 1). In other words, the output is 1 if one of the inputs is
    1, but not both. We will only allow two-input XOR gates.
    """
    def __init__(self, _input_1, _input_2):
        # allow only two inputs
        self._input_1 = _input_1
        self._input_2 = _input_2

    def get_inputs(self):
        return(self._input_1, self._input_2)

    def set_inputs(self, _input_1, _input_2):
        self._input_1 = _input_1
        self._input_2 = _input_2

    def get_output(self):
        NOT_1 = NOT.NOT(self._input_1)
        NOT_1_output = NOT_1.get_output()
        NOT_2 = NOT.NOT(self._input_2)
        NOT_2_output = NOT_2.get_output()

        AND_1 = AND.AND(self._input_2, NOT_1_output)
        AND_1_output = AND_1.get_output()
        AND_2 = AND.AND(self._input_1, NOT_2_output)
        AND_2_output = AND_2.get_output()

        OR_1 = OR.OR(AND_1_output, AND_2_output)
        OR_1_output = OR_1.get_output()

        return OR_1_output
