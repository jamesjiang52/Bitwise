"""
This module defines classes that simulate demultiplexers. A demultiplexer
receives a single input and selects one output to transmit the input to. This
selection is done by one or more select inputs. The other outputs that are not
selected will have the value 0. The demultiplexers in this module have an
additional enable input; if this input is 0, all the output values are 0,
regardless of the other inputs.

The following classes are defined:
    Demultiplexer1To2
    Demultiplexer1To4
    Demultiplexer1To8
    Demultiplexer1To16
"""
import sys
sys.path.insert(0, "../")
import gate
import DEC


class Demultiplexer1To2:
    """
    This demultiplexer has three inputs (one of which is an enable input and
    one of which is a select input) and two outputs:
                    ________
        enable ----|        |---- output_1
        select ----|        |---- output_2
         input ----|________|

    The input is transmitted to output_1 for a (1) select and output_2 for a
    (0) select. If the enable is 0, the outputs are all 0, regardless of input.
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
            select,
            _input
        ) = self._inputs

        NOT_1 = gate.NOT.NOT(select)
        NOT_1_output = NOT_1.get_output()

        AND_1 = gate.AND.AND(enable, select, _input)
        AND_1_output = AND_1.get_output()
        AND_2 = gate.AND.AND(enable, NOT_1_output, _input)
        AND_2_output = AND_2.get_output()

        return(
            AND_1_output,
            AND_2_output
        )


class Demultiplexer1To4:
    """
    This demultiplexer has four inputs (one of which is an enable input and
    two of which are select inputs) and four outputs:
                      ________
          enable ----|        |---- output_1
        select_1 ----|        |---- output_2
        select_2 ----|        |---- output_3
           input ----|________|---- output_4

    The selects have select_1 and select_2 as the MSB and LSB, respectively.
    The input is transmitted to output_1 for a (1, 1) select and output_4 for a
    (0, 0) select. If the enable is 0, the outputs are all 0, regardless of
    input.
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
            select_1,
            select_2,
            _input
        ) = self._inputs

        dec_1 = DEC.Decoder1Of4(_input, select_1, select_2)
        dec_1_output = dec_1.get_output()

        AND_1 = gate.AND.AND(enable, dec_1_output[0])
        AND_1_output = AND_1.get_output()
        AND_2 = gate.AND.AND(enable, dec_1_output[1])
        AND_2_output = AND_2.get_output()
        AND_3 = gate.AND.AND(enable, dec_1_output[2])
        AND_3_output = AND_3.get_output()
        AND_4 = gate.AND.AND(enable, dec_1_output[3])
        AND_4_output = AND_4.get_output()

        return(
            AND_1_output,
            AND_2_output,
            AND_3_output,
            AND_4_output
        )


class Demultiplexer1To8:
    """
    This demultiplexer has five inputs (one of which is an enable input and
    three of which are select inputs) and eight outputs:
                      ________
          enable ----|        |---- output_1
        select_1 ----|        |---- output_2
        select_2 ----|        |---- output_3
        select_3 ----|        |---- output_4
           input ----|        |---- output_5
                     |        |---- output_6
                     |        |---- output_7
                     |________|---- output_8

    The selects have select_1 and select_3 as the MSB and LSB, respectively.
    The input is transmitted to output_1 for a (1, 1, 1) select and output_8
    for a (0, 0, 0) select. If the enable is 0, the outputs are all 0,
    regardless of input.
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
            select_1,
            select_2,
            select_3,
            _input
        ) = self._inputs

        dec_1 = DEC.Decoder1Of8(_input, select_1, select_2, select_3)
        dec_1_output = dec_1.get_output()

        AND_1 = gate.AND.AND(enable, dec_1_output[0])
        AND_1_output = AND_1.get_output()
        AND_2 = gate.AND.AND(enable, dec_1_output[1])
        AND_2_output = AND_2.get_output()
        AND_3 = gate.AND.AND(enable, dec_1_output[2])
        AND_3_output = AND_3.get_output()
        AND_4 = gate.AND.AND(enable, dec_1_output[3])
        AND_4_output = AND_4.get_output()
        AND_5 = gate.AND.AND(enable, dec_1_output[4])
        AND_5_output = AND_5.get_output()
        AND_6 = gate.AND.AND(enable, dec_1_output[5])
        AND_6_output = AND_6.get_output()
        AND_7 = gate.AND.AND(enable, dec_1_output[6])
        AND_7_output = AND_7.get_output()
        AND_8 = gate.AND.AND(enable, dec_1_output[7])
        AND_8_output = AND_8.get_output()

        return(
            AND_1_output,
            AND_2_output,
            AND_3_output,
            AND_4_output,
            AND_5_output,
            AND_6_output,
            AND_7_output,
            AND_8_output
        )


class Demultiplexer1To16:
    """
    This demultiplexer has six inputs (one of which is an enable input and
    four of which are select inputs) and sixteen outputs:
                      ________
          enable ----|        |---- output_1
        select_1 ----|        |---- output_2
        select_2 ----|        |---- output_3
        select_3 ----|        |---- output_4
        select_4 ----|        |---- output_5
           input ----|        |---- output_6
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

    The selects have select_1 and select_4 as the MSB and LSB, respectively.
    The input is transmitted to output_1 for a (1, 1, 1, 1) select and
    output_16 for a (0, 0, 0, 0) select. If the enable is 0, the outputs are
    all 0, regardless of input.
    """
    def __init__(self, *_inputs):
        if len(_inputs) != 6:
            raise TypeError(
                "Expected 6 inputs, received {0}.".format(len(_inputs)))

        for _input in _inputs:
            if (_input != 0) and (_input != 1):
                raise ValueError(
                    "Inputs must be 0 or 1, received \"{0}\".".format(_input))

        self._inputs = _inputs

    def set_inputs(self, *_inputs):
        if len(_inputs) != 6:
            raise TypeError(
                "Expected 6 inputs, received {0}.".format(len(_inputs)))

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
            _input
        ) = self._inputs

        dec_1 = DEC.Decoder1Of16(
            _input,
            select_1,
            select_2,
            select_3,
            select_4
        )
        dec_1_output = dec_1.get_output()

        AND_1 = gate.AND.AND(enable, dec_1_output[0])
        AND_1_output = AND_1.get_output()
        AND_2 = gate.AND.AND(enable, dec_1_output[1])
        AND_2_output = AND_2.get_output()
        AND_3 = gate.AND.AND(enable, dec_1_output[2])
        AND_3_output = AND_3.get_output()
        AND_4 = gate.AND.AND(enable, dec_1_output[3])
        AND_4_output = AND_4.get_output()
        AND_5 = gate.AND.AND(enable, dec_1_output[4])
        AND_5_output = AND_5.get_output()
        AND_6 = gate.AND.AND(enable, dec_1_output[5])
        AND_6_output = AND_6.get_output()
        AND_7 = gate.AND.AND(enable, dec_1_output[6])
        AND_7_output = AND_7.get_output()
        AND_8 = gate.AND.AND(enable, dec_1_output[7])
        AND_8_output = AND_8.get_output()
        AND_9 = gate.AND.AND(enable, dec_1_output[8])
        AND_9_output = AND_9.get_output()
        AND_10 = gate.AND.AND(enable, dec_1_output[9])
        AND_10_output = AND_10.get_output()
        AND_11 = gate.AND.AND(enable, dec_1_output[10])
        AND_11_output = AND_11.get_output()
        AND_12 = gate.AND.AND(enable, dec_1_output[11])
        AND_12_output = AND_12.get_output()
        AND_13 = gate.AND.AND(enable, dec_1_output[12])
        AND_13_output = AND_13.get_output()
        AND_14 = gate.AND.AND(enable, dec_1_output[13])
        AND_14_output = AND_14.get_output()
        AND_15 = gate.AND.AND(enable, dec_1_output[14])
        AND_15_output = AND_15.get_output()
        AND_16 = gate.AND.AND(enable, dec_1_output[15])
        AND_16_output = AND_16.get_output()

        return(
            AND_1_output,
            AND_2_output,
            AND_3_output,
            AND_4_output,
            AND_5_output,
            AND_6_output,
            AND_7_output,
            AND_8_output,
            AND_9_output,
            AND_10_output,
            AND_11_output,
            AND_12_output,
            AND_13_output,
            AND_14_output,
            AND_15_output,
            AND_16_output
        )
