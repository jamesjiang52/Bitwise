class OR:
    """
    This class simulates an OR gate. By definition, the output of an OR gate is
    1 if at least one of its inputs are 1. Otherwise, the output is 0.
    """
    def __init__(self, *_inputs):
        self._inputs = _inputs

    def get_inputs(self):
        return self._inputs

    def set_inputs(self, *_inputs):
        self._inputs = _inputs

    def get_output(self):
        for _input in self._inputs:
            if _input == 1:
                return 1
        return 0
