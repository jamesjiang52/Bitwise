"""
This module defines classes that simulate converters. A converter, as the name
suggests, converts code from one format to another, as long as they are enabled
by the enable input.

The following classes are defined:
    CONVHEXToCCSSD
    CONVHEXToCASSD
    CONVHEXToCCSSDDual
    CONVHEXToCASSDDual
    CONVHEXToCCSSDQuad
    CONVHEXToCASSDQuad
"""
import sys
sys.path.append("../")
import gate


class CONVHEXToCCSSD:
    """
    This converter converts a four-bit input into a seven-segment display with
    a common cathode. It has five inputs (one of which is an enable input) and
    seven outputs:
                     ________
         enable ----|        |---- output_1
        input_1 ----|        |---- output_2
        input_2 ----|        |---- output_3
        input_3 ----|        |---- output_4
        input_4 ----|        |---- output_5
                    |        |---- output_6
                    |________|---- output_7

    The inputs have input_1 and input_4 as the MSB and LSB, respectively.
    Referring to the below diagram, the outputs have output_1 as G, output_2 as
    F, output_3 as E, and so on. Since the LED's have a common cathode, they
    are illuminated by an output of value 1.
        ________
       |   A    |
     F |        | B
       |        |
       |________|
       |   G    |
     E |        | C
       |        |
       |________|
           D

    If the enable is 0, the outputs are all 0, regardless of input.
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

        NOT_1 = gate.NOT.NOT(input_1)
        NOT_1_output = NOT_1.get_output()
        NOT_2 = gate.NOT.NOT(input_2)
        NOT_2_output = NOT_2.get_output()
        NOT_3 = gate.NOT.NOT(input_3)
        NOT_3_output = NOT_3.get_output()
        NOT_4 = gate.NOT.NOT(input_4)
        NOT_4_output = NOT_4.get_output()

        OR_1 = gate.OR.OR(input_1, input_2, input_3, input_4)
        OR_1_output = OR_1.get_output()
        OR_2 = gate.OR.OR(input_1, input_2, input_3, NOT_4_output)
        OR_2_output = OR_2.get_output()
        OR_3 = gate.OR.OR(input_1, input_2, NOT_3_output, input_4)
        OR_3_output = OR_3.get_output()
        OR_4 = gate.OR.OR(input_1, input_2, NOT_3_output, NOT_4_output)
        OR_4_output = OR_4.get_output()
        OR_5 = gate.OR.OR(input_1, NOT_2_output, input_3, input_4)
        OR_5_output = OR_5.get_output()
        OR_6 = gate.OR.OR(input_1, NOT_2_output, input_3, NOT_4_output)
        OR_6_output = OR_6.get_output()
        OR_7 = gate.OR.OR(input_1, NOT_2_output, NOT_3_output, input_4)
        OR_7_output = OR_7.get_output()
        OR_8 = gate.OR.OR(input_1, NOT_2_output, NOT_3_output, NOT_4_output)
        OR_8_output = OR_8.get_output()
        OR_9 = gate.OR.OR(NOT_1_output, input_2, input_3, input_4)
        OR_9_output = OR_9.get_output()
        OR_10 = gate.OR.OR(NOT_1_output, input_2, input_3, NOT_4_output)
        OR_10_output = OR_10.get_output()
        OR_11 = gate.OR.OR(NOT_1_output, input_2, NOT_3_output, input_4)
        OR_11_output = OR_11.get_output()
        OR_12 = gate.OR.OR(NOT_1_output, input_2, NOT_3_output, NOT_4_output)
        OR_12_output = OR_12.get_output()
        OR_13 = gate.OR.OR(NOT_1_output, NOT_2_output, input_3, input_4)
        OR_13_output = OR_13.get_output()
        OR_14 = gate.OR.OR(NOT_1_output, NOT_2_output, input_3, NOT_4_output)
        OR_14_output = OR_14.get_output()
        OR_15 = gate.OR.OR(NOT_1_output, NOT_2_output, NOT_3_output, input_4)
        OR_15_output = OR_15.get_output()
        OR_16 = gate.OR.OR(
            NOT_1_output,
            NOT_2_output,
            NOT_3_output,
            NOT_4_output)
        OR_16_output = OR_16.get_output()

        AND_1 = gate.AND.AND(
            enable,
            OR_2_output,
            OR_5_output,
            OR_12_output,
            OR_14_output
        )
        AND_1_output = AND_1.get_output()
        AND_2 = gate.AND.AND(
            enable,
            OR_6_output,
            OR_7_output,
            OR_12_output,
            OR_13_output,
            OR_15_output,
            OR_16_output
        )
        AND_2_output = AND_2.get_output()
        AND_3 = gate.AND.AND(
            enable,
            OR_3_output,
            OR_13_output,
            OR_15_output,
            OR_16_output
        )
        AND_3_output = AND_3.get_output()
        AND_4 = gate.AND.AND(
            enable,
            OR_2_output,
            OR_5_output,
            OR_8_output,
            OR_11_output,
            OR_16_output
        )
        AND_4_output = AND_4.get_output()
        AND_5 = gate.AND.AND(
            enable,
            OR_2_output,
            OR_4_output,
            OR_5_output,
            OR_6_output,
            OR_8_output,
            OR_10_output
        )
        AND_5_output = AND_5.get_output()
        AND_6 = gate.AND.AND(
            enable,
            OR_2_output,
            OR_3_output,
            OR_4_output,
            OR_8_output,
            OR_14_output
        )
        AND_6_output = AND_6.get_output()
        AND_7 = gate.AND.AND(
            enable,
            OR_1_output,
            OR_2_output,
            OR_8_output,
            OR_13_output
        )
        AND_7_output = AND_7.get_output()

        return(
            AND_7_output,
            AND_6_output,
            AND_5_output,
            AND_4_output,
            AND_3_output,
            AND_2_output,
            AND_1_output
        )


class CONVHEXToCASSD:
    """
    This converter converts a four-bit input into a seven-segment display with
    a common anode. It has five inputs (one of which is an enable input) and
    seven outputs:
                     ________
         enable ----|        |---- output_1
        input_1 ----|        |---- output_2
        input_2 ----|        |---- output_3
        input_3 ----|        |---- output_4
        input_4 ----|        |---- output_5
                    |        |---- output_6
                    |________|---- output_7

    The inputs have input_1 and input_4 as the MSB and LSB, respectively.
    Referring to the below diagram, the outputs have output_1 as G, output_2 as
    F, output_3 as E, and so on. Since the LED's have a common anode, they are
    illuminated by an output of value 0.
        ________
       |   A    |
     F |        | B
       |        |
       |________|
       |   G    |
     E |        | C
       |        |
       |________|
           D

    If the enable is 0, the outputs are all 1, regardless of input.
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
        ssd_1 = CONVHEXToCCSSD(*self._inputs)
        ssd_1_output = ssd_1.get_output()

        NOT_1 = gate.NOT.NOT(ssd_1_output[0])
        NOT_1_output = NOT_1.get_output()
        NOT_2 = gate.NOT.NOT(ssd_1_output[1])
        NOT_2_output = NOT_2.get_output()
        NOT_3 = gate.NOT.NOT(ssd_1_output[2])
        NOT_3_output = NOT_3.get_output()
        NOT_4 = gate.NOT.NOT(ssd_1_output[3])
        NOT_4_output = NOT_4.get_output()
        NOT_5 = gate.NOT.NOT(ssd_1_output[4])
        NOT_5_output = NOT_5.get_output()
        NOT_6 = gate.NOT.NOT(ssd_1_output[5])
        NOT_6_output = NOT_6.get_output()
        NOT_7 = gate.NOT.NOT(ssd_1_output[6])
        NOT_7_output = NOT_7.get_output()

        return(
            NOT_1_output,
            NOT_2_output,
            NOT_3_output,
            NOT_4_output,
            NOT_5_output,
            NOT_6_output,
            NOT_7_output
        )


class CONVHEXToCCSSDDual:
    """
    This converter simply stacks two seven-segment displays with common
    cathodes. It has nine inputs and fourteen outputs:
                     ________
         enable ----|        |---- output_1
        input_1 ----|        |---- output_2
        input_2 ----|        |---- output_3
        input_3 ----|        |---- output_4
        input_4 ----|        |---- output_5
        input_5 ----|        |---- output_6
        input_6 ----|        |---- output_7
        input_7 ----|        |---- output_8
        input_8 ----|        |---- output_9
                    |        |---- output_10
                    |        |---- output_11
                    |        |---- output_12
                    |        |---- output_13
                    |________|---- output_14

    The inputs have input_1 and input_8 as the MSB and LSB, respectively.
    Inputs input_1 to input_4 are converted to outputs output_1 to output_7.
    Inputs input_5 to input_8 are converted to outputs output_8 to output_14.
    If the enable is 0, the outputs are all 0, regardless of input.
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

        ssd_1 = CONVHEXToCCSSD(enable, input_1, input_2, input_3, input_4)
        ssd_1_output = ssd_1.get_output()
        ssd_2 = CONVHEXToCCSSD(enable, input_5, input_6, input_7, input_8)
        ssd_2_output = ssd_2.get_output()

        return ssd_1_output + ssd_2_output


class CONVHEXToCASSDDual:
    """
    This converter simply stacks two seven-segment displays with common anodes.
    It has nine inputs and fourteen outputs:
                     ________
         enable ----|        |---- output_1
        input_1 ----|        |---- output_2
        input_2 ----|        |---- output_3
        input_3 ----|        |---- output_4
        input_4 ----|        |---- output_5
        input_5 ----|        |---- output_6
        input_6 ----|        |---- output_7
        input_7 ----|        |---- output_8
        input_8 ----|        |---- output_9
                    |        |---- output_10
                    |        |---- output_11
                    |        |---- output_12
                    |        |---- output_13
                    |________|---- output_14

    The inputs have input_1 and input_8 as the MSB and LSB, respectively.
    Inputs input_1 to input_4 are converted to outputs output_1 to output_7.
    Inputs input_5 to input_8 are converted to outputs output_8 to output_14.
    If the enable is 0, the outputs are all 1, regardless of input.
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

        ssd_1 = CONVHEXToCASSD(enable, input_1, input_2, input_3, input_4)
        ssd_1_output = ssd_1.get_output()
        ssd_2 = CONVHEXToCASSD(enable, input_5, input_6, input_7, input_8)
        ssd_2_output = ssd_2.get_output()

        return ssd_1_output + ssd_2_output



class CONVHEXToCCSSQuad:
    """
    This converter simply stacks four seven-segment displays with common
    cathodes. It has seventeen inputs and twenty-eight outputs:
                      ________
          enable ----|        |---- output_1
         input_1 ----|        |---- output_2
         input_2 ----|        |---- output_3
         input_3 ----|        |---- output_4
         input_4 ----|        |---- output_5
         input_5 ----|        |---- output_6
         input_6 ----|        |---- output_7
         input_7 ----|        |---- output_8
         input_8 ----|        |---- output_9
         input_9 ----|        |---- output_10
        input_10 ----|        |---- output_11
        input_11 ----|        |---- output_12
        input_12 ----|        |---- output_13
        input_13 ----|        |---- output_14
        input_14 ----|        |---- output_15
        input_15 ----|        |---- output_16
        input_16 ----|        |---- output_17
                     |        |---- output_18
                     |        |---- output_19
                     |        |---- output_20
                     |        |---- output_21
                     |        |---- output_22
                     |        |---- output_23
                     |        |---- output_24
                     |        |---- output_25
                     |        |---- output_26
                     |        |---- output_27
                     |________|---- output_28

    The inputs have input_1 and input_16 as the MSB and LSB, respectively.
    Inputs input_1 to input_4 are converted to outputs output_1 to output_7.
    Inputs input_5 to input_8 are converted to outputs output_8 to output_14.
    Inputs input_9 to input_12 are converted to outputs output_15 to output_21.
    Inputs input_13 to input_16 are converted to outputs output_22 to
    output_28. If the enable is 0, the outputs are all 0, regardless of input.
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

        ssd_1 = CONVHEXToCCSSD(enable, input_1, input_2, input_3, input_4)
        ssd_1_output = ssd_1.get_output()
        ssd_2 = CONVHEXToCCSSD(enable, input_5, input_6, input_7, input_8)
        ssd_2_output = ssd_2.get_output()
        ssd_3 = CONVHEXToCCSSD(enable, input_9, input_10, input_11, input_12)
        ssd_3_output = ssd_3.get_output()
        ssd_4 = CONVHEXToCCSSD(enable, input_13, input_14, input_15, input_16)
        ssd_4_output = ssd_4.get_output()

        return ssd_1_output + ssd_2_output + ssd_3_output + ssd_4_output



class CONVHEXToCASSDQuad:
    """
    This converter simply stacks four seven-segment displays with common
    anodes. It has seventeen inputs and twenty-eight outputs:
                      ________
          enable ----|        |---- output_1
         input_1 ----|        |---- output_2
         input_2 ----|        |---- output_3
         input_3 ----|        |---- output_4
         input_4 ----|        |---- output_5
         input_5 ----|        |---- output_6
         input_6 ----|        |---- output_7
         input_7 ----|        |---- output_8
         input_8 ----|        |---- output_9
         input_9 ----|        |---- output_10
        input_10 ----|        |---- output_11
        input_11 ----|        |---- output_12
        input_12 ----|        |---- output_13
        input_13 ----|        |---- output_14
        input_14 ----|        |---- output_15
        input_15 ----|        |---- output_16
        input_16 ----|        |---- output_17
                     |        |---- output_18
                     |        |---- output_19
                     |        |---- output_20
                     |        |---- output_21
                     |        |---- output_22
                     |        |---- output_23
                     |        |---- output_24
                     |        |---- output_25
                     |        |---- output_26
                     |        |---- output_27
                     |________|---- output_28

    The inputs have input_1 and input_16 as the MSB and LSB, respectively.
    Inputs input_1 to input_4 are converted to outputs output_1 to output_7.
    Inputs input_5 to input_8 are converted to outputs output_8 to output_14.
    Inputs input_9 to input_12 are converted to outputs output_15 to output_21.
    Inputs input_13 to input_16 are converted to outputs output_22 to
    output_28. If the enable is 0, the outputs are all 1, regardless of input.
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

        ssd_1 = CONVHEXToCASSD(enable, input_1, input_2, input_3, input_4)
        ssd_1_output = ssd_1.get_output()
        ssd_2 = CONVHEXToCCSSD(enable, input_5, input_6, input_7, input_8)
        ssd_2_output = ssd_2.get_output()
        ssd_3 = CONVHEXToCASSD(enable, input_9, input_10, input_11, input_12)
        ssd_3_output = ssd_3.get_output()
        ssd_4 = CONVHEXToCCSSD(enable, input_13, input_14, input_15, input_16)
        ssd_4_output = ssd_4.get_output()

        return ssd_1_output + ssd_2_output + ssd_3_output + ssd_4_output
