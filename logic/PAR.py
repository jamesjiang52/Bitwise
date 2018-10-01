"""

"""
import sys
sys.path.insert(0, "../")
import gate


class PAROf4Gen:
    """

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
