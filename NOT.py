class NOT:
    """
    This class simulates a NOT gate, otherwise known as an inverter. The output
    is 1 if the input is 0, and 0 if the input is 1.
    """
    def __init__(self, _input):
        self._input = _input

    def get_input(self):
        return self._input

    def set_input(self, _input):
        self._input = _input

    def get_output(self):
        if self._input == 0:
            return 1
        elif self._input == 1:
            return 0
