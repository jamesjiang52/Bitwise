"""
This module defines classes that simulate parity generators and parity
checkers. These two circuits have essentially the same structure, but perform
different functions. A parity generator transmits a single output, called a
parity bit, based on its inputs. A parity checker transmits a single output
that denotes an error if it has value 1.

All the parity generators in this module are even. If the input carries an even
number of 1's, the parity bit is 0. If the input carries an odd number of 1's,
the parity bit is 1. This ensures that the total number of 1's in the input and
the parity bit is even, which is the premise for a parity checker.

The parity checkers in this module are even. If the input carries an even
number of 1's, there is no error and the output is 0. If the input carries an
odd number of 1's, there has been an error in transmission and the output is 1.

The following classes are defined:
    PAROf4Gen
    PAROf4Check
    PAROf8Gen
    PAROf8Check
    PAROf16Gen
    PAROf16Check
"""
import sys
sys.path.insert(0, "../")
import gate


class PAROf4Gen:
    """
    This parity generator has four inputs and a single output:
                     ________
        input_1 ----|        |---- parity_bit
        input_2 ----|        |
        input_3 ----|        |
        input_4 ----|________|

    If the number of 1's in the input is even, parity_bit is 0. If the number
    of 1's in the input is odd, parity_bit is 1.
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
            input_1,
            input_2,
            input_3,
            input_4
        ) = self._inputs

        XOR_1 = gate.XOR.XOR(input_1, input_2)
        XOR_1_output = XOR_1.get_output()
        XOR_2 = gate.XOR.XOR(input_3, input_4)
        XOR_2_output = XOR_2.get_output()
        XOR_3 = gate.XOR.XOR(XOR_1_output, XOR_2_output)
        XOR_3_output = XOR_3.get_output()

        return XOR_3_output


class PAROf4Check:
    """
    This parity checker has five inputs (one of which is the parity bit) and a
    single output, denoting if an error has occurred:
                        ________
           input_1 ----|        |---- error
           input_2 ----|        |
           input_3 ----|        |
           input_4 ----|        |
        parity_bit ----|________|

    If the number of 1's in the input, including parity_bit, is even, no error
    has occurred and the output is 0. If the number of 1's in the input,
    including parity_bit, is odd, an error has occurred and the output is 1.
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
            input_1,
            input_2,
            input_3,
            input_4,
            parity_bit
        ) = self._inputs

        par_gen_1 = PAROf4Gen(input_1, input_2, input_3, input_4)
        par_gen_1_output = par_gen_1.get_output()
        XOR_1 = gate.XOR.XOR(par_gen_1_output, parity_bit)
        XOR_1_output = XOR_1.get_output()

        return XOR_1_output


class PAROf8Gen:
    """
    This parity generator has eight inputs and a single output:
                     ________
        input_1 ----|        |---- parity_bit
        input_2 ----|        |
        input_3 ----|        |
        input_4 ----|        |
        input_5 ----|        |
        input_6 ----|        |
        input_7 ----|        |
        input_8 ----|________|

    If the number of 1's in the input is even, parity_bit is 0. If the number
    of 1's in the input is odd, parity_bit is 1.
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
        input_1 = self._inputs[0:4]
        input_2 = self._inputs[4:8]

        par_gen_1 = PAROf4Gen(*input_1)
        par_gen_1_output = par_gen_1.get_output()
        par_gen_2 = PAROf4Gen(*input_2)
        par_gen_2_output = par_gen_2.get_output()

        XOR_1 = gate.XOR.XOR(par_gen_1_output, par_gen_2_output)
        XOR_1_output = XOR_1.get_output()

        return XOR_1_output


class PAROf8Check:
    """
    This parity checker has nine inputs (one of which is the parity bit) and a
    single output, denoting if an error has occurred:
                        ________
           input_1 ----|        |---- error
           input_2 ----|        |
           input_3 ----|        |
           input_4 ----|        |
           input_5 ----|        |
           input_6 ----|        |
           input_7 ----|        |
           input_8 ----|        |
        parity_bit ----|________|

    If the number of 1's in the input, including parity_bit, is even, no error
    has occurred and the output is 0. If the number of 1's in the input,
    including parity_bit, is odd, an error has occurred and the output is 1.
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
        input_1 = self._inputs[0:4]
        input_2 = self._inputs[4:8]
        parity_bit = self._inputs[8]

        par_gen_1 = PAROf8Gen(*input_1, *input_2)
        par_gen_1_output = par_gen_1.get_output()
        XOR_1 = gate.XOR.XOR(par_gen_1_output, parity_bit)
        XOR_1_output = XOR_1.get_output()

        return XOR_1_output


class PAROf16Gen:
    """
    This parity generator has sixteen inputs and a single output:
                      ________
         input_1 ----|        |---- parity_bit
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

    If the number of 1's in the input is even, parity_bit is 0. If the number
    of 1's in the input is odd, parity_bit is 1.
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
        input_1 = self._inputs[0:8]
        input_2 = self._inputs[8:16]

        par_gen_1 = PAROf8Gen(*input_1)
        par_gen_1_output = par_gen_1.get_output()
        par_gen_2 = PAROf8Gen(*input_2)
        par_gen_2_output = par_gen_2.get_output()

        XOR_1 = gate.XOR.XOR(par_gen_1_output, par_gen_2_output)
        XOR_1_output = XOR_1.get_output()

        return XOR_1_output


class PAROf16Check:
    """
    This parity checker has seventeen inputs (one of which is the parity bit)
    and a single output, denoting if an error has occurred:
                        ________
           input_1 ----|        |---- error
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
          input_16 ----|        |
        parity_bit ----|________|

    If the number of 1's in the input, including parity_bit, is even, no error
    has occurred and the output is 0. If the number of 1's in the input,
    including parity_bit, is odd, an error has occurred and the output is 1.
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
        input_1 = self._inputs[0:8]
        input_2 = self._inputs[8:16]
        parity_bit = self._inputs[16]

        par_gen_1 = PAROf16Gen(*input_1, *input_2)
        par_gen_1_output = par_gen_1.get_output()
        XOR_1 = gate.XOR.XOR(par_gen_1_output, parity_bit)
        XOR_1_output = XOR_1.get_output()

        return XOR_1_output
