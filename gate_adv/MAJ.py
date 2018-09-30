"""

"""
import sys
sys.path.append("../")
import gate


class MAJOf4:
    """

    """
    def __init__(self, *_inputs):
        if len(_inputs) != 4:
            raise TypeError(
                "Expected 4 inputs, received {0}.".format(len(_inputs)))

        for _input in _inputs:
            if (_input != 0) and (_input != 1):
                raise ValueError(
                    "Inputs must be 0 or 1, received \"{0}\".".format(_input))

        self._inputs = _inputs

    def set_inputs(self, *_inputs):
        if len(_inputs) != 4:
            raise TypeError(
                "Expected 4 inputs, received {0}.".format(len(_inputs)))

        for _input in _inputs:
            if (_input != 0) and (_input != 1):
                raise ValueError(
                    "Inputs must be 0 or 1, received \"{0}\".".format(_input))

        self._inputs = _inputs

    def get_output(self):
        (
            input_1,
            input_2,
            input_3,
            input_4
        ) = self._inputs

        AND_1 = gate.AND.AND(input_1, input_2, input_3)
        AND_1_output = AND_1.get_output()
        AND_2 = gate.AND.AND(input_1, input_2, input_4)
        AND_2_output = AND_2.get_output()
        AND_3 = gate.AND.AND(input_1, input_3, input_4)
        AND_3_output = AND_3.get_output()
        AND_4 = gate.AND.AND(input_2, input_3, input_4)
        AND_4_output = AND_4.get_output()

        OR_1 = gate.OR.OR(
            AND_1_output,
            AND_2_output,
            AND_3_output,
            AND_4_output
        )
        OR_1_output = OR_1.get_output()

        return OR_1_output
