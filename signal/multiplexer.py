"""
This module defines classes that simulate multiplexers. A multiplexer receives
multiple inputs and selects one of them to be the output. This selection is
done by one or more "select" inputs.

The following classes are defined:
    MultiplexerTwoInput
    MultiplexerFourInput
    MultiplexerEightInput
    MultiplexerSixteenInput
"""
import sys
sys.path.append("../")
import gate


class MultiplexerTwoInput:
    """
    This multiplexer has three inputs (one of which is a select input) and a
    single output:
                     ________
         select ----|        |---- output
        input_1 ----|        |
        input_2 ----|________|

    The output takes on the value of input_1 for a (0) select and input_2 for a
    (1) select.
    """
    def __init__(self, *_inputs):
        assert len(_inputs) == 3
        self._inputs = _inputs

    def set_inputs(self, *_inputs):
        assert len(_inputs) == 3
        self._inputs = _inputs

    def get_output(self):
        (
            _select,
            _input_1,
            _input_2
        ) = self._inputs

        NOT_1 = gate.NOT.NOT(_select)
        NOT_1_output = NOT_1.get_output()

        AND_1 = gate.AND.AND(NOT_1_output, _input_1)
        AND_1_output = AND_1.get_output()
        AND_2 = gate.AND.AND(_select, _input_2)
        AND_2_output = AND_2.get_output()

        OR_1 = gate.OR.OR(AND_1_output, AND_2_output)
        OR_1_output = OR_1.get_output()

        return OR_1_output


class MultiplexerFourInput:
    """
    This multiplexer has six inputs (two of which are select inputs) and a
    single output:
                      ________
        select_1 ----|        |---- output
        select_2 ----|        |
         input_1 ----|        |
         input_2 ----|        |
         input_3 ----|        |
         input_4 ----|________|

    The selects have select_1 and select_2 as the MSB and LSB, respectively.
    The output takes on the value of input_1 for a (0, 0) select and input_4
    for a (1, 1) select.
    """
    def __init__(self, *_inputs):
        assert len(_inputs) == 6
        self._inputs = _inputs

    def set_inputs(self, *_inputs):
        assert len(_inputs) == 6
        self._inputs = _inputs

    def get_output(self):
        (
            select_1,
            select_2,
            input_1,
            input_2,
            input_3,
            input_4
        ) = self._inputs

        multiplexer_1 = MultiplexerTwoInput(select_2, input_1, input_2)
        multiplexer_1_output = multiplexer_1.get_output()
        multiplexer_2 = MultiplexerTwoInput(select_2, input_3, input_4)
        multiplexer_2_output = multiplexer_2.get_output()
        multiplexer_3 = MultiplexerTwoInput(
            select_1,
            multiplexer_1_output,
            multiplexer_2_output
        )
        multiplexer_3_output = multiplexer_3.get_output()

        return multiplexer_3_output


class MultiplexerEightInput:
    """
    This multiplexer has eleven inputs (three of which are select inputs) and a
    single output:
                      ________
        select_1 ----|        |---- output
        select_2 ----|        |
        select_3 ----|        |
         input_1 ----|        |
         input_2 ----|        |
         input_3 ----|        |
         input_4 ----|        |
         input_5 ----|        |
         input_6 ----|        |
         input_7 ----|        |
         input_8 ----|________|

    The selects have select_1 and select_3 as the MSB and LSB, respectively.
    The output takes on the value of input_1 for a (0, 0, 0) select and input_8
    for a (1, 1, 1) select.
    """
    def __init__(self, *_inputs):
        assert len(_inputs) == 11
        self._inputs = _inputs

    def set_inputs(self, *_inputs):
        assert len(_inputs) == 11
        self._inputs = _inputs

    def get_output(self):
        (
            select_1,
            select_2,
            select_3,
            input_1,
            input_2,
            input_3,
            input_4,
            input_5,
            input_6,
            input_7,
            input_8
        ) = self._inputs

        multiplexer_1 = MultiplexerFourInput(
            select_2,
            select_3,
            input_1,
            input_2,
            input_3,
            input_4
        )
        multiplexer_1_output = multiplexer_1.get_output()
        multiplexer_2 = MultiplexerFourInput(
            select_2,
            select_3,
            input_5,
            input_6,
            input_7,
            input_8
        )
        multiplexer_2_output = multiplexer_2.get_output()
        multiplexer_3 = MultiplexerTwoInput(
            select_1,
            multiplexer_1_output,
            multiplexer_2_output
        )
        multiplexer_3_output = multiplexer_3.get_output()

        return multiplexer_3_output


class MultiplexerSixteenInput:
    """
    This multiplexer has six inputs (two of which are select inputs) and a
    single output:
                      ________
        select_1 ----|        |---- output
        select_2 ----|        |
        select_3 ----|        |
        select_4 ----|        |
         input_1 ----|        |
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

    The selects have select_1 and select_4 as the MSB and LSB, respectively.
    The output takes on the value of input_1 for a (0, 0, 0, 0) select and
    input_16 for a (0, 0, 0, 0) select.
    """
    def __init__(self, *_inputs):
        assert len(_inputs) == 6
        self._inputs = _inputs

    def set_inputs(self, *_inputs):
        assert len(_inputs) == 6
        self._inputs = _inputs

    def get_output(self):
        (
            select_1,
            select_2,
            select_3,
            select_4,
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

        multiplexer_1 = MultiplexerFourInput(
            select_3,
            select_4,
            input_1,
            input_2,
            input_3,
            input_4
        )
        multiplexer_1_output = multiplexer_1.get_output()
        multiplexer_2 = MultiplexerFourInput(
            select_3,
            select_4,
            input_5,
            input_6,
            input_7,
            input_8
        )
        multiplexer_2_output = multiplexer_2.get_output()
        multiplexer_3 = MultiplexerFourInput(
            select_3,
            select_4,
            input_9,
            input_10,
            input_11,
            input_12
        )
        multiplexer_3_output = multiplexer_3.get_output()
        multiplexer_4 = MultiplexerFourInput(
            select_3,
            select_4,
            input_13,
            input_14,
            input_15,
            input_16
        )
        multiplexer_4_output = multiplexer_4.get_output()
        multiplexer_5 = MultiplexerFourInput(
            select_1,
            select_2,
            multiplexer_1_output,
            multiplexer_2_output,
            multiplexer_3_output,
            multiplexer_4_output
        )
        multiplexer_5_output = multiplexer_5.get_output()

        return multiplexer_5_output
