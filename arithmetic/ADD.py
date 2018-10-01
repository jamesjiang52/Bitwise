"""
This modules defines classes that simulate adders, as well as classes that are
used as building blocks for adders.

The following classes are defined:
    HalfADD
    FullADD
    ADD4And4
"""
import sys
sys.path.insert(0, "../")
import gate


class HalfADD:
    """
    This half-adder has two inputs and two outputs:
                     ________
        input_1 ----|        |---- carry_out
        input_2 ----|________|---- sum

    Inputs input_1 and input_2 are both 1-bit binary numbers, and the half-
    adder computes their sum. The sum is transmitted via the sum output, and
    the carry-out is transmitted via the carry_out output.

    In general, half-adders are not very useful in building larger adders since
    they do not have a carry-in input. It is usually better to use a full-adder
    for this purpose.
    """
    def __init__(self, *_inputs):
        if len(_inputs) != 2:
            raise TypeError(
                "Expected 2 inputs, received {0}.".format(len(_inputs)))

        for _input in _inputs:
            if (_input != 0) and (_input != 1):
                raise ValueError(
                    "Inputs must be 0 or 1, received \"{0}\".".format(_input))

        self._inputs = _inputs

    def set_inputs(self, *_inputs):
        if len(_inputs) != 2:
            raise TypeError(
                "Expected 2 inputs, received {0}.".format(len(_inputs)))

        for _input in _inputs:
            if (_input != 0) and (_input != 1):
                raise ValueError(
                    "Inputs must be 0 or 1, received \"{0}\".".format(_input))

        self._inputs = _inputs

    def get_output(self):
        (
            input_1,
            input_2
        ) = self._inputs

        XOR_1 = gate.XOR.XOR(input_1, input_2)
        XOR_1_output = XOR_1.get_output()

        AND_1 = gate.AND.AND(input_1, input_2)
        AND_1_output = AND_1.get_output()

        return(AND_1_output, XOR_1_output)


class FullADD:
    """
    This full-adder has three inputs and two outputs:
                      ________
        carry_in ----|        |---- carry_out
         input_1 ----|        |---- sum
         input_2 ----|________|

    Inputs input_1, input_2, and carry_in are all 1-bit binary numbers, and the
    half-adder computes their sum. The sum is transmitted via the sum output,
    and the carry-out is transmitted via the carry_out output.

    Because full-adders have a carry-in input, they are far more useful as the
    building blocks for larger adders than half-adders.
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
            carry_in,
            input_1,
            input_2
        ) = self._inputs

        XOR_1 = gate.XOR.XOR(input_1, input_2)
        XOR_1_output = XOR_1.get_output()
        XOR_2 = gate.XOR.XOR(carry_in, XOR_1_output)
        XOR_2_output = XOR_2.get_output()

        AND_1 = gate.AND.AND(input_1, input_2)
        AND_1_output = AND_1.get_output()
        AND_2 = gate.AND.AND(XOR_1_output, carry_in)
        AND_2_output = AND_2.get_output()

        OR_1 = gate.OR.OR(AND_1_output, AND_2_output)
        OR_1_output = OR_1.get_output()

        return(OR_1_output, XOR_2_output)


class ADD4And4:
    """
    This adder has eight inputs and five outputs:
                     ________
        input_1 ----|        |---- carry_out
        input_2 ----|        |---- output_1
        input_3 ----|        |---- output_2
        input_4 ----|        |---- output_3
        input_5 ----|        |---- output_4
        input_6 ----|        |
        input_7 ----|        |
        input_8 ----|________|

    The adder computes the sum of two 4-bit binary numbers. Inputs input_1 and
    input_4 correspond to the MSB and LSB, respectively, of the first number,
    and inputs input_5 and input_8 correspond to the MSB and LSB, respectively,
    of the second number. The outputs have carry_out and output_4 as the MSB
    and LSB, respectively.
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
        (
            input_1,
            input_2,
            input_3,
            input_4,
            input_5,
            input_6,
            input_7,
            input_8
        ) = self._inputs

        fa_1 = FullADD(0, input_4, input_8)
        fa_1_output = fa_1.get_output()
        fa_2 = FullADD(fa_1_output[0], input_3, input_7)
        fa_2_output = fa_2.get_output()
        fa_3 = FullADD(fa_2_output[0], input_2, input_6)
        fa_3_output = fa_3.get_output()
        fa_4 = FullADD(fa_3_output[0], input_1, input_5)
        fa_4_output = fa_4.get_output()

        return(
            fa_4_output[0],
            fa_4_output[1],
            fa_3_output[1],
            fa_2_output[1],
            fa_1_output[1]
        )


class CLA4And4:
    """

    """
    def __init__(self, *_inputs):
        self._inputs = _inputs

    def set_inputs(self, *_inputs):
        self._inputs = _inputs

    def get_output(self):
        (
            carry_in,
            input_1,
            input_2,
            input_3,
            input_4,
            input_5,
            input_6,
            input_7,
            input_8
        ) = self._inputs

        AND_1 = gate.AND.AND(input_4, input_8)
        AND_2 = gate.AND.AND(input_3, input_7)
        AND_3 = gate.AND.AND(input_2, input_6)
        AND_4 = gate.AND.AND(input_1, input_5)

        G_0 = AND_1.get_output()
        G_1 = AND_2.get_output()
        G_2 = AND_3.get_output()
        G_3 = AND_4.get_output()

        OR_1 = gate.OR.OR(input_4, input_8)
        OR_2 = gate.OR.OR(input_3, input_7)
        OR_3 = gate.OR.OR(input_2, input_6)
        OR_4 = gate.OR.OR(input_1, input_5)

        P_0 = OR_1.get_output()
        P_1 = OR_2.get_output()
        P_2 = OR_3.get_output()
        P_3 = OR_4.get_output()

        AND_5 = gate.AND.AND(P_0, P_1, P_2, P_3)
        AND_6 = gate.AND.AND(G_2, P_3)
        AND_6_output = AND_6.get_output()
        AND_7 = gate.AND.AND(G_1, P_2, P_3)
        AND_7_output = AND_7.get_output()
        AND_8 = gate.AND.AND(G_0, P_1, P_2, P_3)
        AND_8_output = AND_8.get_output()

        OR_5 = gate.OR.OR(G_3, AND_6_output, AND_7_output, AND_8_output)

        PG = AND_5.get_output()
        GG = OR_5.get_output()

        AND_9 = gate.AND.AND(PG, carry_in)
        AND_9_output = AND_9.get_output()

        OR_10 = gate.OR.OR(GG, AND_9_output)
        CG = OR_10.get_output()

        return(CG, PG, GG)
