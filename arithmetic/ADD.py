"""
This modules defines classes that simulate adders, as well as classes that are
used as building blocks for adders. These adders are designed to work only with
unsigned integers. For adder/subtractor circuits, refer to the ADD_SUB.py
module.

The following classes are defined:
    HalfAdder
    FullAdder
    LookaheadCarryUnit4
    LookaheadCarryUnit16
    Adder4
    Adder8
    Adder16
"""
import sys
sys.path.insert(0, "../")
import gate


class HalfAdder:
    """
    This class simulates a half-adder, which has two inputs and two outputs:
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


class FullAdder:
    """
    This class simulates a full-adder, which has three inputs and two outputs:
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


class LookaheadCarryUnit4:
    """
    This class simulates a lookahead carry unit for a 4-bit adder. Though an
    LCU is not used in the 4-bit adder itself, it is useful for fast operations
    with chained 4-bit adders. An LCU takes the binary numbers being added
    together and determines if a carry is generated, without waiting for all
    the carries to "ripple" through the chain.

    This LCU has nine inputs and three outputs:
                      ________
        carry_in ----|        |---- carry_out
         input_1 ----|        |---- group_propagate
         input_2 ----|        |---- group_generate
         input_3 ----|        |
         input_4 ----|        |
         input_5 ----|        |
         input_6 ----|        |
         input_7 ----|        |
         input_8 ----|________|

    Inputs input_1 and input_4 correspond to the MSB and LSB, respectively,
    of the first addend, and inputs input_5 and input_8 correspond to the MSB
    and LSB, respectively, of the second addend. The carry-in input is usually
    0 for an adder, but can be 1 if multiple adders are chained together.

    The carry_out output denotes whether or not the two 4-bit binary numbers
    will have a carry-out. The group_propagate and group_generate outputs are
    used in higher-order LCU's and denote, respectively, whether the binary
    numbers will propagate an input carry-in to carry-out, and whether the
    binary numbers will themselves generate a carry-out.
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


class LookaheadCarryUnit16:
    """
    This class simulates a lookahead carry unit for a 16-bit adder. It is
    useful for fast operations, since it takes the binary numbers being added
    together and determines if a carry is generated, without waiting for all
    the carries to "ripple" through the chain of adders.

    This LCU has thirty-three inputs and six outputs:
                      ________
        carry_in ----|        |---- internal_carry_1
         input_1 ----|        |---- internal_carry_2
         input_2 ----|        |---- internal_carry_3
         input_3 ----|        |---- carry_out
         input_4 ----|        |---- group_propagate
             ...     |        |---- group_generate
        input_13 ----|        |
        input_14 ----|        |
        input_15 ----|        |
        input_16 ----|        |
        input_17 ----|        |
        input_18 ----|        |
        input_19 ----|        |
        input_20 ----|        |
             ...
        input_29 ----|        |
        input_30 ----|        |
        input_31 ----|        |
        input_32 ----|________|

    Inputs input_1 and input_16 correspond to the MSB and LSB, respectively,
    of the first addend, and inputs input_17 and input_32 correspond to the MSB
    and LSB, respectively, of the second addend. The carry-in input is usually
    0 for an adder, but can be 1 if multiple adders are chained together.

    The carry_out output denotes whether or not the two 16-bit binary numbers
    will have a carry-out. The internal_carry outputs are used in the 16-bit
    adder to speed up operations. The group_propagate and group_generate
    outputs are used in higher-order LCU's and denote, respectively, whether
    the binary numbers will propagate an input carry-in to carry-out, and
    whether the binary numbers will themselves generate a carry-out.
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
        carry_in = self._inputs[0]
        input_1 = self._inputs[1:17]
        input_2 = self._inputs[17:33]

        lcu_1 = LookaheadCarryUnit4(carry_in, *input_1[12:16], *input_2[12:16])
        lcu_1_output = lcu_1.get_output()
        lcu_2 = LookaheadCarryUnit4(
            lcu_1_output[0],
            *input_1[8:12],
            *input_2[8:12]
        )
        lcu_2_output = lcu_2.get_output()
        lcu_3 = LookaheadCarryUnit4(
            lcu_2_output[0],
            *input_1[4:8],
            *input_2[4:8]
        )
        lcu_3_output = lcu_3.get_output()
        lcu_4 = LookaheadCarryUnit4(
            lcu_3_output[0],
            *input_1[0:4],
            *input_2[0:4]
        )
        lcu_4_output = lcu_4.get_output()

        AND_1 = gate.AND.AND(
            lcu_1_output[1],
            lcu_2_output[1],
            lcu_3_output[1],
            lcu_4_output[1]
        )
        AND_2 = gate.AND.AND(lcu_3_output[2], lcu_4_output[1])
        AND_2_output = AND_2.get_output()
        AND_3 = gate.AND.AND(lcu_2_output[2], lcu_3_output[1], lcu_4_output[1])
        AND_3_output = AND_3.get_output()
        AND_4 = gate.AND.AND(
            lcu_1_output[2],
            lcu_2_output[1],
            lcu_3_output[1],
            lcu_4_output[1]
        )
        AND_4_output = AND_4.get_output()

        OR_5 = gate.OR.OR(
            lcu_4_output[2],
            AND_2_output,
            AND_3_output,
            AND_4_output
        )

        PG = AND_1.get_output()
        GG = OR_5.get_output()

        return(
            lcu_1_output[0],
            lcu_2_output[0],
            lcu_3_output[0],
            lcu_4_output[0],
            PG,
            GG
        )


class Adder4:
    """
    This adder has nine inputs and five outputs:
                      ________
        carry_in ----|        |---- carry_out
         input_1 ----|        |---- output_1
         input_2 ----|        |---- output_2
         input_3 ----|        |---- output_3
         input_4 ----|        |---- output_4
         input_5 ----|        |
         input_6 ----|        |
         input_7 ----|        |
         input_8 ----|________|

    The adder computes the sum of two 4-bit binary numbers. Inputs input_1 and
    input_4 correspond to the MSB and LSB, respectively, of the first addend,
    and inputs input_5 and input_8 correspond to the MSB and LSB, respectively,
    of the second addend. The carry-in input is usually 0 for an adder, but can
    be 1 if multiple adders are chained together. The outputs have carry_out
    and output_4 as the MSB and LSB, respectively.
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

        fa_1 = FullAdder(carry_in, input_4, input_8)
        fa_1_output = fa_1.get_output()
        fa_2 = FullAdder(fa_1_output[0], input_3, input_7)
        fa_2_output = fa_2.get_output()
        fa_3 = FullAdder(fa_2_output[0], input_2, input_6)
        fa_3_output = fa_3.get_output()
        fa_4 = FullAdder(fa_3_output[0], input_1, input_5)
        fa_4_output = fa_4.get_output()

        return(
            fa_4_output[0],
            fa_4_output[1],
            fa_3_output[1],
            fa_2_output[1],
            fa_1_output[1]
        )


class Adder8:
    """
    This adder has seventeen inputs and nine outputs:
                      ________
        carry_in ----|        |---- carry_out
         input_1 ----|        |---- output_1
         input_2 ----|        |---- output_2
         input_3 ----|        |---- output_3
         input_4 ----|        |---- output_4
         input_5 ----|        |---- output_5
         input_6 ----|        |---- output_6
         input_7 ----|        |---- output_7
         input_8 ----|        |---- output_8
         input_9 ----|        |
        input_10 ----|        |
        input_11 ----|        |
        input_12 ----|        |
        input_13 ----|        |
        input_14 ----|        |
        input_15 ----|        |
        input_16 ----|________|

    The adder computes the sum of two 8-bit binary numbers. Inputs input_1 and
    input_8 correspond to the MSB and LSB, respectively, of the first addend,
    and inputs input_9 and input_16 correspond to the MSB and LSB,
    respectively, of the second addend. The carry-in input is usually 0 for an
    adder, but can be 1 if multiple adders are chained together. The outputs
    have carry_out and output_8 as the MSB and LSB, respectively.
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
        carry_in = self._inputs[0]
        input_1 = self._inputs[1:9]
        input_2 = self._inputs[9:17]

        lcu_1 = LookaheadCarryUnit4(carry_in, *input_1[4:8], *input_2[4:8])
        lcu_1_output = lcu_1.get_output()

        add_1 = Adder4(lcu_1_output[0], *input_1[0:4], *input_2[0:4])
        add_1_output = add_1.get_output()
        add_2 = Adder4(carry_in, *input_1[4:8], *input_2[4:8])
        add_2_output = add_2.get_output()

        return(*add_1_output, *add_2_output[1:5])


class Adder16:
    """
    This adder has thirty-three inputs and seventeen outputs:
                      ________
        carry_in ----|        |---- carry_out
         input_1 ----|        |---- output_1
         input_2 ----|        |---- output_2
         input_3 ----|        |---- output_3
         input_4 ----|        |---- output_4
             ...     |        |---- output_5
        input_13 ----|        |---- output_6
        input_14 ----|        |---- output_7
        input_15 ----|        |---- output_8
        input_16 ----|        |---- output_9
        input_17 ----|        |---- output_10
        input_18 ----|        |---- output_11
        input_19 ----|        |---- output_12
        input_20 ----|        |---- output_13
             ...     |        |---- output_14
        input_29 ----|        |---- output_15
        input_30 ----|        |---- output_16
        input_31 ----|        |
        input_32 ----|________|

    The adder computes the sum of two 16-bit binary numbers. Inputs input_1 and
    input_16 correspond to the MSB and LSB, respectively, of the first addend,
    and inputs input_17 and input_32 correspond to the MSB and LSB,
    respectively, of the second addend. The carry-in input is usually 0 for an
    adder, but can be 1 if multiple adders are chained together. The outputs
    have carry_out and output_16 as the MSB and LSB, respectively.
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
        carry_in = self._inputs[0]
        input_1 = self._inputs[1:17]
        input_2 = self._inputs[17:33]

        lcu_1 = LookaheadCarryUnit16(carry_in, *input_1, *input_2)
        lcu_1_output = lcu_1.get_output()

        add_1 = Adder4(lcu_1_output[2], *input_1[0:4], *input_2[0:4])
        add_1_output = add_1.get_output()
        add_2 = Adder4(lcu_1_output[1], *input_1[4:8], *input_2[4:8])
        add_2_output = add_2.get_output()
        add_3 = Adder4(lcu_1_output[0], *input_1[8:12], *input_2[8:12])
        add_3_output = add_3.get_output()
        add_4 = Adder4(carry_in, *input_1[12:16], *input_2[12:16])
        add_4_output = add_4.get_output()

        return(
            lcu_1_output[3],
            *add_1_output,
            *add_2_output,
            *add_3_output,
            *add_4_output
        )
