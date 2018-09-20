import sys
sys.path.append("..")
from multiplexer import MultiplexerFourInput


class TestMultiplexerFourInput:
    def test_multiplexer_four_input_000000(self):
        multiplexer = MultiplexerFourInput(0, 0, 0, 0, 0, 0)
        assert multiplexer.get_output() == 0

    def test_multiplexer_four_input_000001(self):
        multiplexer = MultiplexerFourInput(0, 0, 0, 0, 0, 1)
        assert multiplexer.get_output() == 0

    def test_multiplexer_four_input_000010(self):
        multiplexer = MultiplexerFourInput(0, 0, 0, 0, 1, 0)
        assert multiplexer.get_output() == 0

    def test_multiplexer_four_input_000011(self):
        multiplexer = MultiplexerFourInput(0, 0, 0, 0, 1, 1)
        assert multiplexer.get_output() == 0

    def test_multiplexer_four_input_000100(self):
        multiplexer = MultiplexerFourInput(0, 0, 0, 1, 0, 0)
        assert multiplexer.get_output() == 0

    def test_multiplexer_four_input_000101(self):
        multiplexer = MultiplexerFourInput(0, 0, 0, 1, 0, 1)
        assert multiplexer.get_output() == 0

    def test_multiplexer_four_input_000110(self):
        multiplexer = MultiplexerFourInput(0, 0, 0, 1, 1, 0)
        assert multiplexer.get_output() == 0

    def test_multiplexer_four_input_000111(self):
        multiplexer = MultiplexerFourInput(0, 0, 0, 1, 1, 1)
        assert multiplexer.get_output() == 0

    def test_multiplexer_four_input_001000(self):
        multiplexer = MultiplexerFourInput(0, 0, 1, 0, 0, 0)
        assert multiplexer.get_output() == 1

    def test_multiplexer_four_input_001001(self):
        multiplexer = MultiplexerFourInput(0, 0, 1, 0, 0, 1)
        assert multiplexer.get_output() == 1

    def test_multiplexer_four_input_001010(self):
        multiplexer = MultiplexerFourInput(0, 0, 1, 0, 1, 0)
        assert multiplexer.get_output() == 1

    def test_multiplexer_four_input_001011(self):
        multiplexer = MultiplexerFourInput(0, 0, 1, 0, 1, 1)
        assert multiplexer.get_output() == 1

    def test_multiplexer_four_input_001100(self):
        multiplexer = MultiplexerFourInput(0, 0, 1, 1, 0, 0)
        assert multiplexer.get_output() == 1

    def test_multiplexer_four_input_001101(self):
        multiplexer = MultiplexerFourInput(0, 0, 1, 1, 0, 1)
        assert multiplexer.get_output() == 1

    def test_multiplexer_four_input_001110(self):
        multiplexer = MultiplexerFourInput(0, 0, 1, 1, 1, 0)
        assert multiplexer.get_output() == 1

    def test_multiplexer_four_input_001111(self):
        multiplexer = MultiplexerFourInput(0, 0, 1, 1, 1, 1)
        assert multiplexer.get_output() == 1

    def test_multiplexer_four_input_010000(self):
        multiplexer = MultiplexerFourInput(0, 1, 0, 0, 0, 0)
        assert multiplexer.get_output() == 0

    def test_multiplexer_four_input_010001(self):
        multiplexer = MultiplexerFourInput(0, 1, 0, 0, 0, 1)
        assert multiplexer.get_output() == 0

    def test_multiplexer_four_input_010010(self):
        multiplexer = MultiplexerFourInput(0, 1, 0, 0, 1, 0)
        assert multiplexer.get_output() == 0

    def test_multiplexer_four_input_010011(self):
        multiplexer = MultiplexerFourInput(0, 1, 0, 0, 1, 1)
        assert multiplexer.get_output() == 0

    def test_multiplexer_four_input_010100(self):
        multiplexer = MultiplexerFourInput(0, 1, 0, 1, 0, 0)
        assert multiplexer.get_output() == 1

    def test_multiplexer_four_input_010101(self):
        multiplexer = MultiplexerFourInput(0, 1, 0, 1, 0, 1)
        assert multiplexer.get_output() == 1

    def test_multiplexer_four_input_010110(self):
        multiplexer = MultiplexerFourInput(0, 1, 0, 1, 1, 0)
        assert multiplexer.get_output() == 1

    def test_multiplexer_four_input_010111(self):
        multiplexer = MultiplexerFourInput(0, 1, 0, 1, 1, 1)
        assert multiplexer.get_output() == 1

    def test_multiplexer_four_input_011000(self):
        multiplexer = MultiplexerFourInput(0, 1, 1, 0, 0, 0)
        assert multiplexer.get_output() == 0

    def test_multiplexer_four_input_011001(self):
        multiplexer = MultiplexerFourInput(0, 1, 1, 0, 0, 1)
        assert multiplexer.get_output() == 0

    def test_multiplexer_four_input_011010(self):
        multiplexer = MultiplexerFourInput(0, 1, 1, 0, 1, 0)
        assert multiplexer.get_output() == 0

    def test_multiplexer_four_input_011011(self):
        multiplexer = MultiplexerFourInput(0, 1, 1, 0, 1, 1)
        assert multiplexer.get_output() == 0

    def test_multiplexer_four_input_011100(self):
        multiplexer = MultiplexerFourInput(0, 1, 1, 1, 0, 0)
        assert multiplexer.get_output() == 1

    def test_multiplexer_four_input_011101(self):
        multiplexer = MultiplexerFourInput(0, 1, 1, 1, 0, 1)
        assert multiplexer.get_output() == 1

    def test_multiplexer_four_input_011110(self):
        multiplexer = MultiplexerFourInput(0, 1, 1, 1, 1, 0)
        assert multiplexer.get_output() == 1

    def test_multiplexer_four_input_011111(self):
        multiplexer = MultiplexerFourInput(0, 1, 1, 1, 1, 1)
        assert multiplexer.get_output() == 1

    def test_multiplexer_four_input_100000(self):
        multiplexer = MultiplexerFourInput(1, 0, 0, 0, 0, 0)
        assert multiplexer.get_output() == 0

    def test_multiplexer_four_input_100001(self):
        multiplexer = MultiplexerFourInput(1, 0, 0, 0, 0, 1)
        assert multiplexer.get_output() == 0

    def test_multiplexer_four_input_100010(self):
        multiplexer = MultiplexerFourInput(1, 0, 0, 0, 1, 0)
        assert multiplexer.get_output() == 1

    def test_multiplexer_four_input_100011(self):
        multiplexer = MultiplexerFourInput(1, 0, 0, 0, 1, 1)
        assert multiplexer.get_output() == 1

    def test_multiplexer_four_input_100100(self):
        multiplexer = MultiplexerFourInput(1, 0, 0, 1, 0, 0)
        assert multiplexer.get_output() == 0

    def test_multiplexer_four_input_100101(self):
        multiplexer = MultiplexerFourInput(1, 0, 0, 1, 0, 1)
        assert multiplexer.get_output() == 0

    def test_multiplexer_four_input_100110(self):
        multiplexer = MultiplexerFourInput(1, 0, 0, 1, 1, 0)
        assert multiplexer.get_output() == 1

    def test_multiplexer_four_input_100111(self):
        multiplexer = MultiplexerFourInput(1, 0, 0, 1, 1, 1)
        assert multiplexer.get_output() == 1

    def test_multiplexer_four_input_101000(self):
        multiplexer = MultiplexerFourInput(1, 0, 1, 0, 0, 0)
        assert multiplexer.get_output() == 0

    def test_multiplexer_four_input_101001(self):
        multiplexer = MultiplexerFourInput(1, 0, 1, 0, 0, 1)
        assert multiplexer.get_output() == 0

    def test_multiplexer_four_input_101010(self):
        multiplexer = MultiplexerFourInput(1, 0, 1, 0, 1, 0)
        assert multiplexer.get_output() == 1

    def test_multiplexer_four_input_101011(self):
        multiplexer = MultiplexerFourInput(1, 0, 1, 0, 1, 1)
        assert multiplexer.get_output() == 1

    def test_multiplexer_four_input_101100(self):
        multiplexer = MultiplexerFourInput(1, 0, 1, 1, 0, 0)
        assert multiplexer.get_output() == 0

    def test_multiplexer_four_input_101101(self):
        multiplexer = MultiplexerFourInput(1, 0, 1, 1, 0, 1)
        assert multiplexer.get_output() == 0

    def test_multiplexer_four_input_101110(self):
        multiplexer = MultiplexerFourInput(1, 0, 1, 1, 1, 0)
        assert multiplexer.get_output() == 1

    def test_multiplexer_four_input_101111(self):
        multiplexer = MultiplexerFourInput(1, 0, 1, 1, 1, 1)
        assert multiplexer.get_output() == 1

    def test_multiplexer_four_input_110000(self):
        multiplexer = MultiplexerFourInput(1, 1, 0, 0, 0, 0)
        assert multiplexer.get_output() == 0

    def test_multiplexer_four_input_110001(self):
        multiplexer = MultiplexerFourInput(1, 1, 0, 0, 0, 1)
        assert multiplexer.get_output() == 1

    def test_multiplexer_four_input_110010(self):
        multiplexer = MultiplexerFourInput(1, 1, 0, 0, 1, 0)
        assert multiplexer.get_output() == 0

    def test_multiplexer_four_input_110011(self):
        multiplexer = MultiplexerFourInput(1, 1, 0, 0, 1, 1)
        assert multiplexer.get_output() == 1

    def test_multiplexer_four_input_110100(self):
        multiplexer = MultiplexerFourInput(1, 1, 0, 1, 0, 0)
        assert multiplexer.get_output() == 0

    def test_multiplexer_four_input_110101(self):
        multiplexer = MultiplexerFourInput(1, 1, 0, 1, 0, 1)
        assert multiplexer.get_output() == 1

    def test_multiplexer_four_input_110110(self):
        multiplexer = MultiplexerFourInput(1, 1, 0, 1, 1, 0)
        assert multiplexer.get_output() == 0

    def test_multiplexer_four_input_110111(self):
        multiplexer = MultiplexerFourInput(1, 1, 0, 1, 1, 1)
        assert multiplexer.get_output() == 1

    def test_multiplexer_four_input_111000(self):
        multiplexer = MultiplexerFourInput(1, 1, 1, 0, 0, 0)
        assert multiplexer.get_output() == 0

    def test_multiplexer_four_input_111001(self):
        multiplexer = MultiplexerFourInput(1, 1, 1, 0, 0, 1)
        assert multiplexer.get_output() == 1

    def test_multiplexer_four_input_111010(self):
        multiplexer = MultiplexerFourInput(1, 1, 1, 0, 1, 0)
        assert multiplexer.get_output() == 0

    def test_multiplexer_four_input_111011(self):
        multiplexer = MultiplexerFourInput(1, 1, 1, 0, 1, 1)
        assert multiplexer.get_output() == 1

    def test_multiplexer_four_input_111100(self):
        multiplexer = MultiplexerFourInput(1, 1, 1, 1, 0, 0)
        assert multiplexer.get_output() == 0

    def test_multiplexer_four_input_111101(self):
        multiplexer = MultiplexerFourInput(1, 1, 1, 1, 0, 1)
        assert multiplexer.get_output() == 1

    def test_multiplexer_four_input_111110(self):
        multiplexer = MultiplexerFourInput(1, 1, 1, 1, 1, 0)
        assert multiplexer.get_output() == 0

    def test_multiplexer_four_input_111111(self):
        multiplexer = MultiplexerFourInput(1, 1, 1, 1, 1, 1)
        assert multiplexer.get_output() == 1
