class Wire:
    """
    This class simulates a wire. It has one attribute, value, that can take on
    a single integer value of 0 or 1.
    """
    def __init__(self, _value):
        if ((_value != 0) and (_value != 1)):
            raise ValueError(
                "Wire value must be 0 or 1, received \"{0}\".".format(_value))

        self._value = _value
        self.connections = []

    @property
    def value(self):
        return self._value

    @value.setter
    def value(self, value):
        if ((value != 0) and (value != 1)):
            raise ValueError(
                "Wire value must be 0 or 1, received \"{0}\".".format(_value))

        self._value = value
        for callback in self.connections:
            callback(self._value)

    def bind_to(self, callback):
        self.connections.append(callback)
