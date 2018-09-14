class AND:
    """
    This class simulates an AND gate. By definition, the output of an AND gate
    is 1 if and only if all of its inputs are 1. Otherwise, the output is 0.
    """
    def __init__(self, *_inputs):
        self._inputs = _inputs

    def get_inputs(self):
        return self._inputs

    def set_inputs(self, *_inputs):
        self._inputs = _inputs

    def get_output(self):
        for _input in self._inputs:
            if _input == 0:
                return 0
        return 1
