"""
This module defines classes that simulate encoders. An encoder receives a
one-hot input and encodes it into something like a binary value, transmitting
the output. The encoders in this module have an additional enable input; if
this input is 0, all the output values are 0, regardless of the other inputs.
Also, the encoders have an additional output that is 0 if all of the inputs are
0. Otherwise, this output has a value of 1. In the case that more than one of
the inputs is 1, the input representing the largest value takes priority (e.g.
if two inputs are 1 and represent the values 2 and 13, the output will be the
encoded value of 13).

The following classes are defined:
    ENC4To2
    ENC8To3
    ENC16To4
"""
import sys
sys.path.append("../")
import gate


class ENC4To2:
    """

    """
    def __init__(self, *_inputs):
        assert len(_inputs) == 5
        self._inputs = _inputs

    def set_inputs(self, *_inputs):
        assert len(_inputs) == 5
        self._inputs = _inputs

    def get_output(self):
        (
            enable,
            input_1,
            input_2,
            input_3,
            input_4
        ) = self._inputs

        NOT_1 = gate.NOT.NOT(input_2)
        NOT_1_output = NOT_1.get_output()

        AND_1 = gate.AND.AND(NOT_1_output, input_3)
        AND_1_output = AND_1.get_output()

        OR_1 = gate.OR.OR(input_1, input_2)
        OR_1_output = OR_1.get_output()
        OR_2 = gate.OR.OR(AND_1_output, input_1)
        OR_2_output = OR_2.get_output()
        OR_3 = gate.OR.OR(input_1, input_2, input_3, input_4)
        OR_3_output = OR_3.get_output()

        AND_2 = gate.AND.AND(enable, OR_3_output)
        AND_2_output = AND_2.get_output()
        AND_3 = gate.AND.AND(enable, OR_1_output)
        AND_3_output = AND_3.get_output()
        AND_4 = gate.AND.AND(enable, OR_2_output)
        AND_4_output = AND_4.get_output()

        return(
            AND_2_output,
            AND_3_output,
            AND_4_output
        )


class ENC8To3:
    """

    """
    def __init__(self, *_inputs):
        assert len(_inputs) == 9
        self._inputs = _inputs

    def set_inputs(self, *_inputs):
        assert len(_inputs) == 9
        self._inputs = _inputs

    def get_outputs(self):
        (
            enable,
            input_1,
            input_2,
            input_3,
            input_4,
            input_5,
            input_6,
            input_7,
            input_8
        ) = self._inputs

        enc_1 = ENC4To2(enable, input_1, input_2, input_3, input_4)
        enc_1_output = enc_1.get_output()
        enc_2 = ENC4To2(enable, input_5, input_6, input_7, input_8)
        enc_2_output = enc_2.get_output()
