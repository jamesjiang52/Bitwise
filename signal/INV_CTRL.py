"""
This module defines classes that simulate controlled inverters. A controlled
inverter inverts all of its inputs when an enable input is set to 1. Otherwise,
the inputs pass through the inverter unchanged.

The following classes are defined:
    ControlledInverter4
    ControlledInverter8
    ControlledInverter16
"""
import sys
sys.path.insert(0, "../")
import gate


class ControlledInverter4:
    """
    This controlled inverter has five inputs (one of which is an enable input)
    and four outputs:
                     ________
         enable ----|        |---- output_1
        input_1 ----|        |---- output_2
        input_2 ----|        |---- output_3
        input_3 ----|        |---- output_4
        input_4 ----|________|

    Output output_1 corresponds to input input_1, output_2 corresponds to
    input_2, and so on.
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
            input_4,
        ) = self._inputs

        XOR_1 = gate.XOR.XOR(enable, input_1)
        XOR_1_output = XOR_1.get_output()
        XOR_2 = gate.XOR.XOR(enable, input_2)
        XOR_2_output = XOR_2.get_output()
        XOR_3 = gate.XOR.XOR(enable, input_3)
        XOR_3_output = XOR_3.get_output()
        XOR_4 = gate.XOR.XOR(enable, input_4)
        XOR_4_output = XOR_4.get_output()

        return(XOR_1_output, XOR_2_output, XOR_3_output, XOR_4_output)


class ControlledInverter8:
    """
    This controlled inverter has nine inputs (one of which is an enable input)
    and eight outputs:
                     ________
         enable ----|        |---- output_1
        input_1 ----|        |---- output_2
        input_2 ----|        |---- output_3
        input_3 ----|        |---- output_4
        input_4 ----|        |---- output_5
        input_5 ----|        |---- output_6
        input_6 ----|        |---- output_7
        input_7 ----|        |---- output_8
        input_8 ----|________|

    Output output_1 corresponds to input input_1, output_2 corresponds to
    input_2, and so on.
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

        inv_1 = ControlledInverter4(
            enable,
            input_1,
            input_2,
            input_3,
            input_4
        )
        inv_1_output = inv_1.get_output()
        inv_2 = ControlledInverter4(
            enable,
            input_5,
            input_6,
            input_7,
            input_8
        )
        inv_2_output = inv_2.get_output()

        return(*inv_1_output, *inv_2_output)


class ControlledInverter16:
    """
    This controlled inverter has seventeen inputs (one of which is an enable
    input) and sixteen outputs:
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
        input_16 ----|________|

    Output output_1 corresponds to input input_1, output_2 corresponds to
    input_2, and so on.
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
        enable = self._inputs[0]
        input_1 = self._inputs[1:9]
        input_2 = self._inputs[9:17]

        inv_1 = ControlledInverter8(enable, *input_1)
        inv_1_output = inv_1.get_output()
        inv_2 = ControlledInverter8(enable, *input_2)
        inv_2_output = inv_2.get_output()

        return(*inv_1_output, *inv_2_output)
