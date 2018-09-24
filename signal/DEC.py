"""
This module defines classes that simulate decoders. A decoder receives an
encoded value, such as a binary value, and transmits a one-hot output
corresponding to the decoded value. The decoders in this module have an
additional enable input; if this input is 0, all the output values are 0,
regardless of the other inputs.

The following classes are defined:
    DEC1Of4
    DEC1Of8
    DEC1Of16
"""
import sys
sys.path.append("../")
import gate


class DEC1Of4:
    """
    This decoder has three inputs (one of which is an enable input) and four
    outputs:
                     ________
         enable ----|        |---- output_1
        input_1 ----|        |---- output_2
        input_2 ----|        |---- output_3
                    |________|---- output_4

    The inputs have input_1 and input_2 as the MSB and LSB, respectively. The
    outputs have output_1 corresponding to the (1, 1) input and output_4
    corresponding to the (0, 0) input. If the enable is 0, the outputs are
    all 0, regardless of input.
    """
    def __init__(self, *_inputs):
        if len(_inputs) != 3:
            raise TypeError(
                "Expected 3 inputs, received {0}.".format(len(_inputs)))

        for _input in _inputs:
            if (_input != 0) and (_input != 1):
                raise ValueError(
                    "Inputs must be 0 or 1, received \"{0}\".".format(_input))

        self._inputs = _inputs

    def set_inputs(self, *_inputs):
        if len(_inputs) != 3:
            raise TypeError(
                "Expected 3 inputs, received {0}.".format(len(_inputs)))

        for _input in _inputs:
            if (_input != 0) and (_input != 1):
                raise ValueError(
                    "Inputs must be 0 or 1, received \"{0}\".".format(_input))

        self._inputs = _inputs

    def get_output(self):
        (
            enable,
            input_1,
            input_2
        ) = self._inputs

        NOT_1 = gate.NOT.NOT(input_1)
        NOT_1_output = NOT_1.get_output()
        NOT_2 = gate.NOT.NOT(input_2)
        NOT_2_output = NOT_2.get_output()

        AND_1 = gate.AND.AND(enable, input_1, input_2)
        AND_1_output = AND_1.get_output()
        AND_2 = gate.AND.AND(enable, input_1, NOT_2_output)
        AND_2_output = AND_2.get_output()
        AND_3 = gate.AND.AND(enable, NOT_1_output, input_2)
        AND_3_output = AND_3.get_output()
        AND_4 = gate.AND.AND(enable, NOT_1_output, NOT_2_output)
        AND_4_output = AND_4.get_output()

        return(
            AND_1_output,
            AND_2_output,
            AND_3_output,
            AND_4_output
        )


class DEC1Of8:
    """
    This decoder has four inputs (one of which is an enable input) and eight
    outputs:
                     ________
         enable ----|        |---- output_1
        input_1 ----|        |---- output_2
        input_2 ----|        |---- output_3
        input_3 ----|        |---- output_4
                    |        |---- output_5
                    |        |---- output_6
                    |        |---- output_7
                    |________|---- output_8

    The inputs have input_1 and input_3 as the MSB and LSB, respectively. The
    outputs have output_1 corresponding to the (1, 1, 1) input and output_8
    corresponding to the (0, 0, 0) input. If the enable is 0, the outputs are
    all 0, regardless of input.
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
            enable,
            input_1,
            input_2,
            input_3
        ) = self._inputs

        NOT_1 = gate.NOT.NOT(input_1)
        NOT_1_output = NOT_1.get_output()

        AND_1 = gate.AND.AND(enable, input_1)
        AND_1_output = AND_1.get_output()
        AND_2 = gate.AND.AND(enable, NOT_1_output)
        AND_2_output = AND_2.get_output()

        dec_1 = DEC1Of4(AND_1_output, input_2, input_3)
        dec_1_output = dec_1.get_output()
        dec_2 = DEC1Of4(AND_2_output, input_2, input_3)
        dec_2_output = dec_2.get_output()

        output_1, output_2, output_3, output_4 = dec_1_output
        output_5, output_6, output_7, output_8 = dec_2_output

        return(
            output_1,
            output_2,
            output_3,
            output_4,
            output_5,
            output_6,
            output_7,
            output_8
        )


class DEC1Of16:
    """
    This decoder has five inputs (one of which is an enable input) and sixteen
    outputs:
                     ________
         enable ----|        |---- output_1
        input_1 ----|        |---- output_2
        input_2 ----|        |---- output_3
        input_3 ----|        |---- output_4
        input_4 ----|        |---- output_5
                    |        |---- output_6
                    |        |---- output_7
                    |        |---- output_8
                    |        |---- output_9
                    |        |---- output_10
                    |        |---- output_11
                    |        |---- output_12
                    |        |---- output_13
                    |        |---- output_14
                    |        |---- output_15
                    |________|---- output_16

    The inputs have input_1 and input_4 as the MSB and LSB, respectively. The
    outputs have output_1 corresponding to the (1, 1, 1, 1) input and
    output_16 corresponding to the (0, 0, 0, 0) input. If the enable is 0, the
    outputs are all 0, regardless of input.
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

        dec_1 = DEC1Of4(enable, input_1, input_2)
        dec_1_output = dec_1.get_output()
        (
            dec_1_output_1,
            dec_1_output_2,
            dec_1_output_3,
            dec_1_output_4
        ) = dec_1_output

        dec_2 = DEC1Of4(dec_1_output_1, input_3, input_4)
        dec_2_output = dec_2.get_output()
        dec_3 = DEC1Of4(dec_1_output_2, input_3, input_4)
        dec_3_output = dec_3.get_output()
        dec_4 = DEC1Of4(dec_1_output_3, input_3, input_4)
        dec_4_output = dec_4.get_output()
        dec_5 = DEC1Of4(dec_1_output_4, input_3, input_4)
        dec_5_output = dec_5.get_output()

        output_1, output_2, output_3, output_4 = dec_2_output
        output_5, output_6, output_7, output_8 = dec_3_output
        output_9, output_10, output_11, output_12 = dec_4_output
        output_13, output_14, output_15, output_16 = dec_5_output

        return(
            output_1,
            output_2,
            output_3,
            output_4,
            output_5,
            output_6,
            output_7,
            output_8,
            output_9,
            output_10,
            output_11,
            output_12,
            output_13,
            output_14,
            output_15,
            output_16
        )
