from AND import AND
from OR import OR
from NOT import NOT


class XNOR:
    """
    This class simulates an XNOR gate. By definition, the output of an XNOR
    gate is 1 if the inputs are the same (i.e. both 0 or both 1), and 0 if the
    inputs are different. In other words, the output is 0 if one of the inputs
    is 1, but not both. We will only allow two-input XOR gates.

    This XNOR gate has two inputs and a single output:
                     ________
        input_1 ----|        |---- output
        input_2 ----|________|

    """
    def __init__(self, *_inputs):
        if len(_inputs) != 2:
            raise TypeError(
                "Expected 2 inputs, received {0}.".format(len(_inputs)))

        for _input in _inputs:
            if (_input != 0) and (_input != 1):
                raise ValueError(
                    "Inputs must be 0 or 1, received \"{0}\".".format(_input))

        self._inputs = _inputs

    def set_inputs(self, *_inputs):
        if len(_inputs) != 2:
            raise TypeError(
                "Expected 2 inputs, received {0}.".format(len(_inputs)))

        for _input in _inputs:
            if (_input != 0) and (_input != 1):
                raise ValueError(
                    "Inputs must be 0 or 1, received \"{0}\".".format(_input))

        self._inputs = _inputs

    def get_output(self):
        input_1, input_2 = self._inputs

        NOT_1 = NOT(input_1)
        NOT_1_output = NOT_1.get_output()
        NOT_2 = NOT(input_2)
        NOT_2_output = NOT_2.get_output()

        AND_1 = AND(input_1, input_2)
        AND_1_output = AND_1.get_output()
        AND_2 = AND(NOT_1_output, NOT_2_output)
        AND_2_output = AND_2.get_output()

        OR_1 = OR(AND_1_output, AND_2_output)
        OR_1_output = OR_1.get_output()

        return OR_1_output
