"""
This module defines classes that simulate decoders. A decoder receives an
encoded value, such as a binary value, and transmits a one-hot output
corresponding to the decoded value.

The following classes are defined:
    DecoderTwoInput
    DecoderThreeInput
    DecoderFourInput
"""
import sys
sys.path.append("../")
import gate


class DecoderTwoInput:
    """
    This decoder has three inputs (one of which is an active-high enable) and
    four outputs:
                     ________
         enable ----|        |---- output_1
        input_1 ----|        |---- output_2
        input_2 ----|        |---- output_3
                    |________|---- output_4

    The inputs have input_1 and input_2 as the MSB and LSB, respectively. The
    outputs have output_1 corresponding to the (1, 1) input and output_4
    corresponding to the (0, 0) input.
    """
    def __init__(self, *_inputs):
        assert len(_inputs) == 3
        self._inputs = _inputs

    def set_inputs(self, *_inputs):
        assert len(_inputs) == 3
        self._inputs = _inputs

    def get_output(self):
        (
            enable,
            input_1,
            input_2
        ) = self._inputs

        NOT_1 = gate.NOT.NOT(input_1)
        NOT_1_output = NOT_1.get_output()
        NOT_2 = gate.NOT.NOT(input_2)
        NOT_2_output = NOT_2.get_output()

        AND_1 = gate.AND.AND(enable, input_1, input_2)
        AND_1_output = AND_1.get_output()
        AND_2 = gate.AND.AND(enable, input_1, NOT_2_output)
        AND_2_output = AND_2.get_output()
        AND_3 = gate.AND.AND(enable, NOT_1_output, input_2)
        AND_3_output = AND_3.get_output()
        AND_4 = gate.AND.AND(enable, NOT_1_output, NOT_2_output)
        AND_4_output = AND_4.get_output()

        return(
            AND_1_output,
            AND_2_output,
            AND_3_output,
            AND_4_output
        )


class DecoderThreeInput:
    """
    This decoder has four inputs (one of which is an active-high enable) and
    eight outputs:
                     ________
         enable ----|        |---- output_1
        input_1 ----|        |---- output_2
        input_2 ----|        |---- output_3
        input_3 ----|        |---- output_4
                    |        |---- output_5
                    |        |---- output_6
                    |        |---- output_7
                    |________|---- output_8

    The inputs have input_1 and input_3 as the MSB and LSB, respectively. The
    outputs have output_1 corresponding to the (1, 1, 1) input and output_8
    corresponding to the (0, 0, 0) input.
    """
    def __init__(self, *_inputs):
        assert len(_inputs) == 4
        self._inputs = _inputs

    def set_inputs(self, *_inputs):
        assert len(_inputs) == 4
        self._inputs = _inputs

    def get_output(self):
        (
            enable,
            input_1,
            input_2,
            input_3
        ) = self._inputs

        NOT_1 = gate.NOT.NOT(input_1)
        NOT_1_output = NOT_1.get_output()

        AND_1 = gate.AND.AND(enable, input_1)
        AND_1_output = AND_1.get_output()
        AND_2 = gate.AND.AND(enable, NOT_1_output)
        AND_2_output = AND_2.get_output()

        decoder_1 = DecoderTwoInput(AND_1_output, input_2, input_3)
        decoder_1_output = decoder_1.get_output()
        decoder_2 = DecoderTwoInput(AND_2_output, input_2, input_3)
        decoder_2_output = decoder_2.get_output()

        output_1, output_2, output_3, output_4 = decoder_1_output
        output_5, output_6, output_7, output_8 = decoder_2_output

        return(
            output_1,
            output_2,
            output_3,
            output_4,
            output_5,
            output_6,
            output_7,
            output_8
        )


class DecoderFourInput:
    """
    This decoder has five inputs (one of which is an active-high enable) and
    sixteen outputs:
                     ________
         enable ----|        |---- output_1
        input_1 ----|        |---- output_2
        input_2 ----|        |---- output_3
        input_3 ----|        |---- output_4
        input_4 ----|        |---- output_5
                    |        |---- output_6
                    |        |---- output_7
                    |        |---- output_8
                    |        |---- output_9
                    |        |---- output_10
                    |        |---- output_11
                    |        |---- output_12
                    |        |---- output_13
                    |        |---- output_14
                    |        |---- output_15
                    |________|---- output_16

    The inputs have input_1 and input_4 as the MSB and LSB, respectively. The
    outputs have output_1 corresponding to the (1, 1, 1, 1) input and
    output_16 corresponding to the (0, 0, 0, 0) input.
    """
    def __init__(self, *_inputs):
        assert len(_inputs) == 5
        self._inputs = _inputs

    def set_inputs(self, *_inputs):
        assert len(_inputs) == 5
        self._inputs = _inputs

    def get_output(self):
        (
            enable,
            input_1,
            input_2,
            input_3,
            input_4
        ) = self._inputs

        decoder_1 = DecoderTwoInput(enable, input_1, input_2)
        decoder_1_output = decoder_1.get_output()
        (
            decoder_1_output_1,
            decoder_1_output_2,
            decoder_1_output_3,
            decoder_1_output_4
        ) = decoder_1_output

        decoder_2 = DecoderTwoInput(decoder_1_output_1, input_3, input_4)
        decoder_2_output = decoder_2.get_output()
        decoder_3 = DecoderTwoInput(decoder_1_output_2, input_3, input_4)
        decoder_3_output = decoder_3.get_output()
        decoder_4 = DecoderTwoInput(decoder_1_output_3, input_3, input_4)
        decoder_4_output = decoder_4.get_output()
        decoder_5 = DecoderTwoInput(decoder_1_output_4, input_3, input_4)
        decoder_5_output = decoder_5.get_output()

        output_1, output_2, output_3, output_4 = decoder_2_output
        output_5, output_6, output_7, output_8 = decoder_3_output
        output_9, output_10, output_11, output_12 = decoder_4_output
        output_13, output_14, output_15, output_16 = decoder_5_output

        return(
            output_1,
            output_2,
            output_3,
            output_4,
            output_5,
            output_6,
            output_7,
            output_8,
            output_9,
            output_10,
            output_11,
            output_12,
            output_13,
            output_14,
            output_15,
            output_16
        )
