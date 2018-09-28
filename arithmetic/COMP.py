"""
This module defines classes that simulate comparators. A comparator receives
two binary numbers as input and, as the name suggests, compares them. There are
many comparators that exist, but the ones in this module simply check if two
binary numbers are equal.

The following classes are defined:
    COMPEQ2
    COMPEQ4
    COMPEQ8
    COMPEQ16
"""
import sys
sys.path.append("../")
import gate


class COMPEQ2:
    """
    This comparator has four inputs and a single output:
                     ________
        input_1 ----|        |---- output
        input_2 ----|        |
        input_3 ----|        |
        input_4 ----|________|

    The first binary number has input_1 and input_2 as the MSB and LSB,
    respectively. The second binary number has input_3 and input_4 as the MSB
    and LSB, respectively. The output is 1 if the two numbers are exactly
    equal, otherwise 0.
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


class COMPEQ4:
    """
    This comparator has eight inputs and a single output:
                     ________
        input_1 ----|        |---- output
        input_2 ----|        |
        input_3 ----|        |
        input_4 ----|        |
        input_5 ----|        |
        input_6 ----|        |
        input_7 ----|        |
        input_8 ----|________|

    The first binary number has input_1 and input_4 as the MSB and LSB,
    respectively. The second binary number has input_5 and input_8 as the MSB
    and LSB, respectively. The output is 1 if the two numbers are exactly
    equal, otherwise 0.
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


class COMPEQ8:
    """
    This comparator has sixteen inputs and a single output:
                      ________
         input_1 ----|        |---- output
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

    The first binary number has input_1 and input_8 as the MSB and LSB,
    respectively. The second binary number has input_9 and input_16 as the MSB
    and LSB, respectively. The output is 1 if the two numbers are exactly
    equal, otherwise 0.
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


class COMPEQ16:
    """
    This comparator has thirty-two inputs and a single output:
                      ________
         input_1 ----|        |---- output
         input_2 ----|        |
         input_3 ----|        |
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

    The first binary number has input_1 and input_16 as the MSB and LSB,
    respectively. The second binary number has input_17 and input_32 as the MSB
    and LSB, respectively. The output is 1 if the two numbers are exactly
    equal, otherwise 0.
    """
    def __init__(self, *_inputs):
        if len(_inputs) != 3:
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
