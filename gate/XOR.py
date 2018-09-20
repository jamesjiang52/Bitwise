from AND import AND
from OR import OR
from NOT import NOT


class XOR:
    """
    This class simulates an XOR gate. By definition, the output of an XOR gate
    is 1 if the inputs are different, and 0 if the inputs are the same (i.e.
    both 0 or both 1). In other words, the output is 1 if one of the inputs is
    1, but not both. We will only allow two-input XOR gates.

    This XOR gate receives two inputs and transmits a single input:
                     ________
        input_1 ----|        |---- output
        input_2 ----|________|

    """
    def __init__(self, *_inputs):
        assert len(_inputs) == 2
        self._inputs = _inputs

    def set_inputs(self, *_inputs):
        assert len(_inputs) == 2
        self._inputs = _inputs

    def get_output(self):
        input_1, input_2 = self._inputs

        NOT_1 = NOT(input_1)
        NOT_1_output = NOT_1.get_output()
        NOT_2 = NOT(input_2)
        NOT_2_output = NOT_2.get_output()

        AND_1 = AND(input_2, NOT_1_output)
        AND_1_output = AND_1.get_output()
        AND_2 = AND(input_1, NOT_2_output)
        AND_2_output = AND_2.get_output()

        OR_1 = OR(AND_1_output, AND_2_output)
        OR_1_output = OR_1.get_output()

        return OR_1_output
