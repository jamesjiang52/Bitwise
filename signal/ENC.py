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
import MUX


class ENC4To2:
    """
    This priority encoder has five inputs (one of which is an enable input) and
    three outputs (one of which is a valid output):
                     ________
         enable ----|        |---- valid
        input_1 ----|        |---- output_1
        input_2 ----|        |---- output_2
        input_3 ----|        |
        input_4 ----|________|

    The outputs have output_1 and output_2 as the MSB and LSB, respectively.
    The output takes on the value of (1, 1) for a one-hot input_1 input and
    (0, 0) for a one-hot input_4 input. If the enable is 0, the outputs are all
    0, regardless of input.
    """
    def __init__(self, *_inputs):
        if len(_inputs) != 5:
            raise TypeError(
                "Expected 5 inputs, received {0}.".format(len(_inputs)))

        for _input in _inputs:
            if (_input != 0) and (_input != 1):
                raise ValueError(
                    "Inputs must be 0 or 1, received \"{0}\".".format(_input))

        self._inputs = _inputs

    def set_inputs(self, *_inputs):
        if len(_inputs) != 5:
            raise TypeError(
                "Expected 5 inputs, received {0}.".format(len(_inputs)))

        for _input in _inputs:
            if (_input != 0) and (_input != 1):
                raise ValueError(
                    "Inputs must be 0 or 1, received \"{0}\".".format(_input))

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
    This priority encoder has nine inputs (one of which is an enable input) and
    four outputs (one of which is a valid output):
                     ________
         enable ----|        |---- valid
        input_1 ----|        |---- output_1
        input_2 ----|        |---- output_2
        input_3 ----|        |---- output_3
        input_4 ----|        |
        input_5 ----|        |
        input_6 ----|        |
        input_7 ----|        |
        input_8 ----|________|

    The outputs have output_1 and output_3 as the MSB and LSB, respectively.
    The output takes on the value of (1, 1, 1) for a one-hot input_1 input and
    (0, 0, 0) for a one-hot input_8 input. If the enable is 0, the outputs are
    all 0, regardless of input.
    """
    def __init__(self, *_inputs):
        if len(_inputs) != 9:
            raise TypeError(
                "Expected 9 inputs, received {0}.".format(len(_inputs)))

        for _input in _inputs:
            if (_input != 0) and (_input != 1):
                raise ValueError(
                    "Inputs must be 0 or 1, received \"{0}\".".format(_input))

        self._inputs = _inputs

    def set_inputs(self, *_inputs):
        if len(_inputs) != 9:
            raise TypeError(
                "Expected 9 inputs, received {0}.".format(len(_inputs)))

        for _input in _inputs:
            if (_input != 0) and (_input != 1):
                raise ValueError(
                    "Inputs must be 0 or 1, received \"{0}\".".format(_input))

        self._inputs = _inputs

    def get_output(self):
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

        mux_1 = MUX.MUX2To1(
            enable,
            enc_1_output[0],
            enc_1_output[1],
            enc_2_output[1]
        )
        mux_1_output = mux_1.get_output()
        mux_2 = MUX.MUX2To1(
            enable,
            enc_1_output[0],
            enc_1_output[2],
            enc_2_output[2]
        )
        mux_2_output = mux_2.get_output()

        OR_1 = gate.OR.OR(enc_1_output[0], enc_2_output[0])
        OR_1_output = OR_1.get_output()

        return(
            OR_1_output,
            enc_1_output[0],
            mux_1_output,
            mux_2_output
        )


class ENC16To4:
    """
    This priority encoder has seventeen inputs (one of which is an enable
    input) and five outputs (one of which is a valid output):
                      ________
          enable ----|        |---- valid
         input_1 ----|        |---- output_1
         input_2 ----|        |---- output_2
         input_3 ----|        |---- output_3
         input_4 ----|        |---- output_4
         input_5 ----|        |
         input_6 ----|        |
         input_7 ----|        |
         input_8 ----|        |
         input_9 ----|        |
        input_10 ----|        |
        input_11 ----|        |
        input_12 ----|        |
        input_13 ----|        |
        input_14 ----|        |
        input_15 ----|        |
        input_16 ----|________|

    The outputs have output_1 and output_4 as the MSB and LSB, respectively.
    The output takes on the value of (1, 1, 1, 1) for a one-hot input_1 input
    and (0, 0, 0, 0) for a one-hot input_16 input. If the enable is 0, the
    outputs are all 0, regardless of input.
    """
    def __init__(self, *_inputs):
        if len(_inputs) != 17:
            raise TypeError(
                "Expected 17 inputs, received {0}.".format(len(_inputs)))

        for _input in _inputs:
            if (_input != 0) and (_input != 1):
                raise ValueError(
                    "Inputs must be 0 or 1, received \"{0}\".".format(_input))

        self._inputs = _inputs

    def set_inputs(self, *_inputs):
        if len(_inputs) != 17:
            raise TypeError(
                "Expected 17 inputs, received {0}.".format(len(_inputs)))

        for _input in _inputs:
            if (_input != 0) and (_input != 1):
                raise ValueError(
                    "Inputs must be 0 or 1, received \"{0}\".".format(_input))

        self._inputs = _inputs

    def get_output(self):
        (
            enable,
            input_1,
            input_2,
            input_3,
            input_4,
            input_5,
            input_6,
            input_7,
            input_8,
            input_9,
            input_10,
            input_11,
            input_12,
            input_13,
            input_14,
            input_15,
            input_16
        ) = self._inputs

        enc_1 = ENC8To3(
            enable,
            input_1,
            input_2,
            input_3,
            input_4,
            input_5,
            input_6,
            input_7,
            input_8
        )
        enc_1_output = enc_1.get_output()
        enc_2 = ENC8To3(
            enable,
            input_9,
            input_10,
            input_11,
            input_12,
            input_13,
            input_14,
            input_15,
            input_16
        )
        enc_2_output = enc_2.get_output()

        mux_1 = MUX.MUX2To1(
            enable,
            enc_1_output[0],
            enc_1_output[1],
            enc_2_output[1]
        )
        mux_1_output = mux_1.get_output()
        mux_2 = MUX.MUX2To1(
            enable,
            enc_1_output[0],
            enc_1_output[2],
            enc_2_output[2]
        )
        mux_2_output = mux_2.get_output()
        mux_3 = MUX.MUX2To1(
            enable,
            enc_1_output[0],
            enc_1_output[3],
            enc_2_output[3]
        )
        mux_3_output = mux_3.get_output()

        OR_1 = gate.OR.OR(enc_1_output[0], enc_2_output[0])
        OR_1_output = OR_1.get_output()

        return(
            OR_1_output,
            enc_1_output[0],
            mux_1_output,
            mux_2_output,
            mux_3_output
        )
