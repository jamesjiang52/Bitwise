"""
This modules defines classes that simulate adder/subtractor circuits. Unlike
the adders in ADD.py, these circuits are designed to work with both unsigned
and signed integers, and can perform both addition and subtraction operations.
This choice is made via an add_subtract select input.

The following classes are defined:
    ADDSUB4
    ADDSUB8
    ADDSUB16
"""
import sys
sys.path.insert(0, "../")
import gate
import signal
import ADD


class ADDSUB4:
    """
    This adder/subtractor has nine inputs (one of which is an add_subtract
    select) and six outputs (one of which is an overflow indicator):
                          ________
        add_subtract ----|        |---- overflow
             input_1 ----|        |---- carry_out
             input_2 ----|        |---- output_1
             input_3 ----|        |---- output_2
             input_4 ----|        |---- output_3
             input_5 ----|        |---- output_4
             input_6 ----|        |
             input_7 ----|        |
             input_8 ----|________|

    If the add_subtract select is set to 0, this circuit computes the sum of
    two 4-bit numbers. Inputs input_1 and input_4 correspond to the MSB and
    LSB, respectively, of the first addend, and inputs input_5 and input_8
    correspond to the MSB and LSB, respectively, of the second addend. The
    outputs will then have carry_out and output_4 as the MSB and LSB,
    respectively. The overflow output for a 0 add_subtract select is
    meaningless.

    If the add_subtract select is set to 1, this circuit computes the
    difference between two signed 3-bit numbers in two's complement form.
    Inputs input_1 and input_5 correspond to the sign bit of the minuend and
    the subtrahend, respectively. The minuend has input_2 as the MSB and
    input_4 as the LSB. The subtrahend has input_6 as the MSB and input_8 as
    the LSB. The outputs will then have output_2 and output_4 as the MSB and
    LSB, respectively, with output_1 being the sign bit. The overflow denotes
    whether or not arithmetic overflow has occurred and the outputs should be
    discarded; it is 1 if overflow has occurred, and 0 otherwise. The carry_out
    output for a 1 add_subtract select is meaningless.
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
        add_sub = self._inputs[0]
        input_1 = self._inputs[1:5]
        input_2 = self._inputs[5:9]

        inv_1 = signal.INV_CTRL.ControlledINV4(add_sub, *input_2)
        inv_1_output = inv_1.get_output()

        add_1 = ADD.ADD4(add_sub, *input_1, *inv_1_output)
        add_1_output = add_1.get_output()

        NOT_1 = gate.NOT.NOT(add_1_output[1])
        NOT_1_output = NOT_1.get_output()
        NOT_2 = gate.NOT.NOT(input_1[0])
        NOT_2_output = NOT_2.get_output()
        NOT_3 = gate.NOT.NOT(input_2[0])
        NOT_3_output = NOT_3.get_output()

        AND_1 = gate.AND.AND(input_1[0], input_2[0], NOT_1_output)
        AND_1_output = AND_1.get_output()
        AND_2 = gate.AND.AND(NOT_2_output, NOT_3_output, add_1_output[1])
        AND_2_output = AND_2.get_output()

        OR_1 = gate.OR.OR(AND_1_output, AND_2_output)
        OR_1_output = OR_1.get_output()

        return(OR_1_output, *add_1_output)


class ADDSUB8:
    """
    This adder/subtractor has seventeen inputs (one of which is an add_subtract
    select) and ten outputs (one of which is an overflow indicator):
                          ________
        add_subtract ----|        |---- overflow
             input_1 ----|        |---- carry_out
             input_2 ----|        |---- output_1
             input_3 ----|        |---- output_2
             input_4 ----|        |---- output_3
             input_5 ----|        |---- output_4
             input_6 ----|        |---- output_5
             input_7 ----|        |---- output_6
             input_8 ----|        |---- output_7
             input_9 ----|        |---- output_8
            input_10 ----|        |
            input_11 ----|        |
            input_12 ----|        |
            input_13 ----|        |
            input_14 ----|        |
            input_15 ----|        |
            input_16 ----|________|

    If the add_subtract select is set to 0, this circuit computes the sum of
    two 8-bit numbers. Inputs input_1 and input_8 correspond to the MSB and
    LSB, respectively, of the first addend, and inputs input_9 and input_16
    correspond to the MSB and LSB, respectively, of the second addend. The
    outputs will then have carry_out and output_8 as the MSB and LSB,
    respectively. The overflow output for a 0 add_subtract select is
    meaningless.

    If the add_subtract select is set to 1, this circuit computes the
    difference between two signed 7-bit numbers in two's complement form.
    Inputs input_1 and input_9 correspond to the sign bit of the minuend and
    the subtrahend, respectively. The minuend has input_2 as the MSB and
    input_8 as the LSB. The subtrahend has input_10 as the MSB and input_16 as
    the LSB. The outputs will then have output_2 and output_8 as the MSB and
    LSB, respectively, with output_1 being the sign bit. The overflow denotes
    whether or not arithmetic overflow has occurred and the outputs should be
    discarded; it is 1 if overflow has occurred, and 0 otherwise. The carry_out
    output for a 1 add_subtract select is meaningless.
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
        add_sub = self._inputs[0]
        input_1 = self._inputs[1:9]
        input_2 = self._inputs[9:17]

        inv_1 = signal.INV_CTRL.ControlledINV8(add_sub, *input_2)
        inv_1_output = inv_1.get_output()

        add_1 = ADD.ADD8(add_sub, *input_1, *inv_1_output)
        add_1_output = add_1.get_output()

        NOT_1 = gate.NOT.NOT(add_1_output[1])
        NOT_1_output = NOT_1.get_output()
        NOT_2 = gate.NOT.NOT(input_1[0])
        NOT_2_output = NOT_2.get_output()
        NOT_3 = gate.NOT.NOT(input_2[0])
        NOT_3_output = NOT_3.get_output()

        AND_1 = gate.AND.AND(input_1[0], input_2[0], NOT_1_output)
        AND_1_output = AND_1.get_output()
        AND_2 = gate.AND.AND(NOT_2_output, NOT_3_output, add_1_output[1])
        AND_2_output = AND_2.get_output()

        OR_1 = gate.OR.OR(AND_1_output, AND_2_output)
        OR_1_output = OR_1.get_output()

        return(OR_1_output, *add_1_output)


class ADDSUB16:
    """
    This adder/subtractor has thirty-three inputs (one of which is an
    add_subtract select) and eighteen outputs (one of which is an overflow
    indicator):
                          ________
        add_subtract ----|        |---- overflow
             input_1 ----|        |---- carry_out
             input_2 ----|        |---- output_1
             input_3 ----|        |---- output_2
             input_4 ----|        |---- output_3
                 ...     |        |---- output_4
            input_13 ----|        |---- output_5
            input_14 ----|        |---- output_6
            input_15 ----|        |---- output_7
            input_16 ----|        |---- output_8
            input_17 ----|        |---- output_9
            input_18 ----|        |---- output_10
            input_19 ----|        |---- output_11
            input_20 ----|        |---- output_12
                 ...     |        |---- output_13
            input_29 ----|        |---- output_14
            input_30 ----|        |---- output_15
            input_31 ----|        |---- output_16
            input_32 ----|________|

    If the add_subtract select is set to 0, this circuit computes the sum of
    two 16-bit numbers. Inputs input_1 and input_16 correspond to the MSB and
    LSB, respectively, of the first addend, and inputs input_17 and input_32
    correspond to the MSB and LSB, respectively, of the second addend. The
    outputs will then have carry_out and output_16 as the MSB and LSB,
    respectively. The overflow output for a 0 add_subtract select is
    meaningless.

    If the add_subtract select is set to 1, this circuit computes the
    difference between two signed 15-bit numbers in two's complement form.
    Inputs input_1 and input_17 correspond to the sign bit of the minuend and
    the subtrahend, respectively. The minuend has input_2 as the MSB and
    input_16 as the LSB. The subtrahend has input_18 as the MSB and input_32 as
    the LSB. The outputs will then have output_2 and output_16 as the MSB and
    LSB, respectively, with output_1 being the sign bit. The overflow denotes
    whether or not arithmetic overflow has occurred and the outputs should be
    discarded; it is 1 if overflow has occurred, and 0 otherwise. The carry_out
    output for a 1 add_subtract select is meaningless.
    """
    def __init__(self, *_inputs):
        if len(_inputs) != 33:
            raise TypeError(
                "Expected 33 inputs, received {0}.".format(len(_inputs)))

        for _input in _inputs:
            if (_input != 0) and (_input != 1):
                raise ValueError(
                    "Inputs must be 0 or 1, received \"{0}\".".format(_input))

        self._inputs = _inputs

    def set_inputs(self, *_inputs):
        if len(_inputs) != 33:
            raise TypeError(
                "Expected 33 inputs, received {0}.".format(len(_inputs)))

        for _input in _inputs:
            if (_input != 0) and (_input != 1):
                raise ValueError(
                    "Inputs must be 0 or 1, received \"{0}\".".format(_input))

        self._inputs = _inputs

    def get_output(self):
        add_sub = self._inputs[0]
        input_1 = self._inputs[1:17]
        input_2 = self._inputs[17:33]

        inv_1 = signal.INV_CTRL.ControlledINV16(add_sub, *input_2)
        inv_1_output = inv_1.get_output()

        add_1 = ADD.ADD16(add_sub, *input_1, *inv_1_output)
        add_1_output = add_1.get_output()

        NOT_1 = gate.NOT.NOT(add_1_output[1])
        NOT_1_output = NOT_1.get_output()
        NOT_2 = gate.NOT.NOT(input_1[0])
        NOT_2_output = NOT_2.get_output()
        NOT_3 = gate.NOT.NOT(input_2[0])
        NOT_3_output = NOT_3.get_output()

        AND_1 = gate.AND.AND(input_1[0], input_2[0], NOT_1_output)
        AND_1_output = AND_1.get_output()
        AND_2 = gate.AND.AND(NOT_2_output, NOT_3_output, add_1_output[1])
        AND_2_output = AND_2.get_output()

        OR_1 = gate.OR.OR(AND_1_output, AND_2_output)
        OR_1_output = OR_1.get_output()

        return(OR_1_output, *add_1_output)
