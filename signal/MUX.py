"""
This module defines classes that simulate multiplexers. A multiplexer receives
multiple inputs and selects one of them to be the output. This selection is
done by one or more select inputs. The multiplexers in this module have an
additional enable input; if this input is 0, all the output values are 0,
regardless of the other inputs.

The following classes are defined:
    MUX2To1
    MUX4To1
    MUX8To1
    MUX16To1
"""
import sys
sys.path.append("../")
import gate


class MUX2To1:
    """
    This multiplexer has four inputs (one of which is an enable input and one
    of which is a select input) and a single output:
                     ________
         enable ----|        |---- output
         select ----|        |
        input_1 ----|        |
        input_2 ----|________|

    The output takes on the value of input_2 for a (0) select and input_1 for a
    (1) select. If the enable is 0, the outputs are all 0, regardless of input.
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
            select,
            input_1,
            input_2
        ) = self._inputs

        NOT_1 = gate.NOT.NOT(select)
        NOT_1_output = NOT_1.get_output()

        AND_1 = gate.AND.AND(select, input_1)
        AND_1_output = AND_1.get_output()
        AND_2 = gate.AND.AND(NOT_1_output, input_2)
        AND_2_output = AND_2.get_output()

        OR_1 = gate.OR.OR(AND_1_output, AND_2_output)
        OR_1_output = OR_1.get_output()

        AND_3 = gate.AND.AND(enable, OR_1_output)
        AND_3_output = AND_3.get_output()

        return AND_3_output


class MUX4To1:
    """
    This multiplexer has seven inputs (one of which is an enable input and two
    of which are select inputs) and a single output:
                      ________
          enable ----|        |---- output
        select_1 ----|        |
        select_2 ----|        |
         input_1 ----|        |
         input_2 ----|        |
         input_3 ----|        |
         input_4 ----|________|

    The selects have select_1 and select_2 as the MSB and LSB, respectively.
    The output takes on the value of input_4 for a (0, 0) select and input_1
    for a (1, 1) select. If the enable is 0, the output is 0, regardless of
    input.
    """
    def __init__(self, *_inputs):
        if len(_inputs) != 7:
            raise TypeError(
                "Expected 7 inputs, received {0}.".format(len(_inputs)))

        for _input in _inputs:
            if (_input != 0) and (_input != 1):
                raise ValueError(
                    "Inputs must be 0 or 1, received \"{0}\".".format(_input))

        self._inputs = _inputs

    def set_inputs(self, *_inputs):
        if len(_inputs) != 7:
            raise TypeError(
                "Expected 7 inputs, received {0}.".format(len(_inputs)))

        for _input in _inputs:
            if (_input != 0) and (_input != 1):
                raise ValueError(
                    "Inputs must be 0 or 1, received \"{0}\".".format(_input))

        self._inputs = _inputs

    def get_output(self):
        (
            enable,
            select_1,
            select_2,
            input_1,
            input_2,
            input_3,
            input_4
        ) = self._inputs

        mux_1 = MUX2To1(enable, select_2, input_1, input_2)
        mux_1_output = mux_1.get_output()
        mux_2 = MUX2To1(enable, select_2, input_3, input_4)
        mux_2_output = mux_2.get_output()
        mux_3 = MUX2To1(enable, select_1, mux_1_output, mux_2_output)
        mux_3_output = mux_3.get_output()

        return mux_3_output


class MUX8To1:
    """
    This multiplexer has twelve inputs (one of which is an enable input and
    three of which are select inputs) and a single output:
                      ________
          enable ----|        |---- output
        select_1 ----|        |
        select_2 ----|        |
        select_3 ----|        |
         input_1 ----|        |
         input_2 ----|        |
         input_3 ----|        |
         input_4 ----|        |
         input_5 ----|        |
         input_6 ----|        |
         input_7 ----|        |
         input_8 ----|________|

    The selects have select_1 and select_3 as the MSB and LSB, respectively.
    The output takes on the value of input_8 for a (0, 0, 0) select and input_1
    for a (1, 1, 1) select. If the enable is 0, the output is 0, regardless of
    input.
    """
    def __init__(self, *_inputs):
        if len(_inputs) != 12:
            raise TypeError(
                "Expected 12 inputs, received {0}.".format(len(_inputs)))

        for _input in _inputs:
            if (_input != 0) and (_input != 1):
                raise ValueError(
                    "Inputs must be 0 or 1, received \"{0}\".".format(_input))

        self._inputs = _inputs

    def set_inputs(self, *_inputs):
        if len(_inputs) != 12:
            raise TypeError(
                "Expected 12 inputs, received {0}.".format(len(_inputs)))

        for _input in _inputs:
            if (_input != 0) and (_input != 1):
                raise ValueError(
                    "Inputs must be 0 or 1, received \"{0}\".".format(_input))

        self._inputs = _inputs

    def get_output(self):
        (
            enable,
            select_1,
            select_2,
            select_3,
            input_1,
            input_2,
            input_3,
            input_4,
            input_5,
            input_6,
            input_7,
            input_8
        ) = self._inputs

        mux_1 = MUX4To1(
            enable,
            select_2,
            select_3,
            input_1,
            input_2,
            input_3,
            input_4
        )
        mux_1_output = mux_1.get_output()
        mux_2 = MUX4To1(
            enable,
            select_2,
            select_3,
            input_5,
            input_6,
            input_7,
            input_8
        )
        mux_2_output = mux_2.get_output()
        mux_3 = MUX2To1(enable, select_1, mux_1_output, mux_2_output)
        mux_3_output = mux_3.get_output()

        return mux_3_output


class MUX16To1:
    """
    This multiplexer has twenty-one inputs (one of which is an enable input and
    four of which are select inputs) and a single output:
                      ________
          enable ----|        |---- output
        select_1 ----|        |
        select_2 ----|        |
        select_3 ----|        |
        select_4 ----|        |
         input_1 ----|        |
         input_2 ----|        |
         input_3 ----|        |
         input_4 ----|        |
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

    The selects have select_1 and select_4 as the MSB and LSB, respectively.
    The output takes on the value of input_16 for a (0, 0, 0, 0) select and
    input_1 for a (1, 1, 1, 1) select. If the enable is 0, the output is 0,
    regardless of input.
    """
    def __init__(self, *_inputs):
        if len(_inputs) != 21:
            raise TypeError(
                "Expected 21 inputs, received {0}.".format(len(_inputs)))

        for _input in _inputs:
            if (_input != 0) and (_input != 1):
                raise ValueError(
                    "Inputs must be 0 or 1, received \"{0}\".".format(_input))

        self._inputs = _inputs

    def set_inputs(self, *_inputs):
        if len(_inputs) != 21:
            raise TypeError(
                "Expected 21 inputs, received {0}.".format(len(_inputs)))

        for _input in _inputs:
            if (_input != 0) and (_input != 1):
                raise ValueError(
                    "Inputs must be 0 or 1, received \"{0}\".".format(_input))

        self._inputs = _inputs

    def get_output(self):
        (
            enable,
            select_1,
            select_2,
            select_3,
            select_4,
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

        mux_1 = MUX4To1(
            enable,
            select_3,
            select_4,
            input_1,
            input_2,
            input_3,
            input_4
        )
        mux_1_output = mux_1.get_output()
        mux_2 = MUX4To1(
            enable,
            select_3,
            select_4,
            input_5,
            input_6,
            input_7,
            input_8
        )
        mux_2_output = mux_2.get_output()
        mux_3 = MUX4To1(
            enable,
            select_3,
            select_4,
            input_9,
            input_10,
            input_11,
            input_12
        )
        mux_3_output = mux_3.get_output()
        mux_4 = MUX4To1(
            enable,
            select_3,
            select_4,
            input_13,
            input_14,
            input_15,
            input_16
        )
        mux_4_output = mux_4.get_output()
        mux_5 = MUX4To1(
            enable,
            select_1,
            select_2,
            mux_1_output,
            mux_2_output,
            mux_3_output,
            mux_4_output
        )
        mux_5_output = mux_5.get_output()

        return mux_5_output
