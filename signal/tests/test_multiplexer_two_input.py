import sys
sys.path.append("..")
from multiplexer import MultiplexerTwoInput


class TestMultiplexerTwoInput:
    def test_multiplexer_two_input_000(self):
        multiplexer = MultiplexerTwoInput(0, 0, 0)
        assert multiplexer.get_output() == 0

    def test_multiplexer_two_input_001(self):
        multiplexer = MultiplexerTwoInput(0, 0, 1)
        assert multiplexer.get_output() == 0

    def test_multiplexer_two_input_010(self):
        multiplexer = MultiplexerTwoInput(0, 1, 0)
        assert multiplexer.get_output() == 1

    def test_multiplexer_two_input_011(self):
        multiplexer = MultiplexerTwoInput(0, 1, 1)
        assert multiplexer.get_output() == 1

    def test_multiplexer_two_input_100(self):
        multiplexer = MultiplexerTwoInput(1, 0, 0)
        assert multiplexer.get_output() == 0

    def test_multiplexer_two_input_101(self):
        multiplexer = MultiplexerTwoInput(1, 0, 1)
        assert multiplexer.get_output() == 1

    def test_multiplexer_two_input_110(self):
        multiplexer = MultiplexerTwoInput(1, 1, 0)
        assert multiplexer.get_output() == 0

    def test_multiplexer_two_input_111(self):
        multiplexer = MultiplexerTwoInput(1, 1, 1)
        assert multiplexer.get_output() == 1
