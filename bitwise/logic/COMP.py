"""
This module defines classes that simulate logical comparators. While there are
many different forms of comparators, the ones in this module simply receive two
signed binary numbers in two's complement form and determine if the first is
greater than, equal to, or less than the second.

The following classes are defined:
    Comparator3
    Comparator7
    Comparator15
"""
import sys
sys.path.insert(0, "../")
import gate
import arithmetic


class Comparator3:
    """
    This logical comparator has eight inputs and three outputs:
                     ________
        input_1 ----|        |---- greater_than
        input_2 ----|        |---- equal_to
        input_3 ----|        |---- less_than
        input_4 ----|        |
        input_5 ----|        |
        input_6 ----|        |
        input_7 ----|        |
        input_8 ----|________|

    The comparator compares two signed 3-bit binary numbers. The first number
    has input_1, input_2, and input_4 as the sign bit, MSB, and LSB,
    respectively. The second number has input_5, input_6, and input_8 as the
    sign bit, MSB, and LSB, respectively. If the first number is greater than
    the second, the greater_than output will be 1, with all other outputs 0.
    The other outputs work similarly.
    """
    def __init__(self, *_inputs):
        if len(_inputs) != 8:
            raise TypeError(
                "Expected 8 inputs, received {0}.".format(len(_inputs)))

        for _input in _inputs:
            if (_input != 0) and (_input != 1):
                raise ValueError(
                    "Inputs must be 0 or 1, received \"{0}\".".format(_input))

        self._inputs = _inputs

    def set_inputs(self, *_inputs):
        if len(_inputs) != 8:
            raise TypeError(
                "Expected 8 inputs, received {0}.".format(len(_inputs)))

        for _input in _inputs:
            if (_input != 0) and (_input != 1):
                raise ValueError(
                    "Inputs must be 0 or 1, received \"{0}\".".format(_input))

        self._inputs = _inputs

    def get_output(self):
        add_sub_1 = arithmetic.ADD_SUB.Adder4Subtractor3(1, *self._inputs)
        add_sub_1_output = add_sub_1.get_output()

        V = add_sub_1_output[0]
        N = add_sub_1_output[2]

        NOR_1 = gate.NOR.NOR(
            add_sub_1_output[2],
            add_sub_1_output[3],
            add_sub_1_output[4],
            add_sub_1_output[5]
        )
        Z = NOR_1.get_output()

        XOR_1 = gate.XOR.XOR(N, V)
        lt = XOR_1.get_output()

        OR_1 = gate.OR.OR(lt, Z)
        OR_1_output = OR_1.get_output()

        NOT_1 = gate.NOT.NOT(OR_1_output)
        gt = NOT_1.get_output()

        return(gt, Z, lt)


class Comparator7:
    """
    This logical comparator has sixteen inputs and three outputs:
                      ________
         input_1 ----|        |---- greater_than
         input_2 ----|        |---- equal_to
         input_3 ----|        |---- less_than
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

    The comparator compares two signed 7-bit binary numbers. The first number
    has input_1, input_2, and input_8 as the sign bit, MSB, and LSB,
    respectively. The second number has input_9, input_10, and input_16 as the
    sign bit, MSB, and LSB, respectively. If the first number is greater than
    the second, the greater_than output will be 1, with all other outputs 0.
    The other outputs work similarly.
    """
    def __init__(self, *_inputs):
        if len(_inputs) != 16:
            raise TypeError(
                "Expected 16 inputs, received {0}.".format(len(_inputs)))

        for _input in _inputs:
            if (_input != 0) and (_input != 1):
                raise ValueError(
                    "Inputs must be 0 or 1, received \"{0}\".".format(_input))

        self._inputs = _inputs

    def set_inputs(self, *_inputs):
        if len(_inputs) != 16:
            raise TypeError(
                "Expected 16 inputs, received {0}.".format(len(_inputs)))

        for _input in _inputs:
            if (_input != 0) and (_input != 1):
                raise ValueError(
                    "Inputs must be 0 or 1, received \"{0}\".".format(_input))

        self._inputs = _inputs

    def get_output(self):
        add_sub_1 = arithmetic.ADD_SUB.Adder8Subtractor7(1, *self._inputs)
        add_sub_1_output = add_sub_1.get_output()

        V = add_sub_1_output[0]
        N = add_sub_1_output[2]

        NOR_1 = gate.NOR.NOR(
            add_sub_1_output[2],
            add_sub_1_output[3],
            add_sub_1_output[4],
            add_sub_1_output[5],
            add_sub_1_output[6],
            add_sub_1_output[7],
            add_sub_1_output[8],
            add_sub_1_output[9]
        )
        Z = NOR_1.get_output()

        XOR_1 = gate.XOR.XOR(N, V)
        lt = XOR_1.get_output()

        OR_1 = gate.OR.OR(lt, Z)
        OR_1_output = OR_1.get_output()

        NOT_1 = gate.NOT.NOT(OR_1_output)
        gt = NOT_1.get_output()

        return(gt, Z, lt)


class Comparator15:
    """
    This logical comparator has thirty-two inputs and three outputs:
                      ________
         input_1 ----|        |---- greater_than
         input_2 ----|        |---- equal_to
         input_3 ----|        |---- less_than
         input_4 ----|        |
             ...     |        |
        input_13 ----|        |
        input_14 ----|        |
        input_15 ----|        |
        input_16 ----|        |
        input_17 ----|        |
        input_18 ----|        |
        input_19 ----|        |
        input_20 ----|        |
             ...     |        |
        input_29 ----|        |
        input_30 ----|        |
        input_31 ----|        |
        input_32 ----|________|

    The comparator compares two signed 15-bit binary numbers. The first number
    has input_1, input_2, and input_16 as the sign bit, MSB, and LSB,
    respectively. The second number has input_17, input_18, and input_32 as the
    sign bit, MSB, and LSB, respectively. If the first number is greater than
    the second, the greater_than output will be 1, with all other outputs 0.
    The other outputs work similarly.
    """
    def __init__(self, *_inputs):
        if len(_inputs) != 32:
            raise TypeError(
                "Expected 32 inputs, received {0}.".format(len(_inputs)))

        for _input in _inputs:
            if (_input != 0) and (_input != 1):
                raise ValueError(
                    "Inputs must be 0 or 1, received \"{0}\".".format(_input))

        self._inputs = _inputs

    def set_inputs(self, *_inputs):
        if len(_inputs) != 32:
            raise TypeError(
                "Expected 32 inputs, received {0}.".format(len(_inputs)))

        for _input in _inputs:
            if (_input != 0) and (_input != 1):
                raise ValueError(
                    "Inputs must be 0 or 1, received \"{0}\".".format(_input))

        self._inputs = _inputs

    def get_output(self):
        add_sub_1 = arithmetic.ADD_SUB.Adder16Subtractor15(1, *self._inputs)
        add_sub_1_output = add_sub_1.get_output()

        V = add_sub_1_output[0]
        N = add_sub_1_output[2]

        NOR_1 = gate.NOR.NOR(add_sub_1_output[2:18])
        Z = NOR_1.get_output()

        XOR_1 = gate.XOR.XOR(N, V)
        lt = XOR_1.get_output()

        OR_1 = gate.OR.OR(lt, Z)
        OR_1_output = OR_1.get_output()

        NOT_1 = gate.NOT.NOT(OR_1_output)
        gt = NOT_1.get_output()

        return(gt, Z, lt)
