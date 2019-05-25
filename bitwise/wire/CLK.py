"""
The following classes are defined:
    Clock
"""


class Clock:
    """Initialize a new clock with value 0. After initialization, the value of
    the clock can be both accessed and mutated using clock.value.

    Raises:
        ValueError: If value assigned to clock is not 0 or 1.
    """
    def __init__(self, value=0):
        if ((value != 0) and (value != 1)):
            raise ValueError(
                "Clock value must be 0 or 1, received \"{0}\".".format(value))

        self._value = value
        self.connections = []

    @property
    def value(self):
        return self._value

    @value.setter
    def value(self, value):
        if ((value != 0) and (value != 1)):
            raise ValueError(
                "Clock value must be 0 or 1, received \"{0}\".".format(value))

        if value != self._value:
            self._value = value
            for callback in self.connections:
                callback(self._value)

    def _bind_to(self, callback):
        if callback not in self.connections:
            self.connections.append(callback)

    def __str__(self):
        return str(self._value)

    def __call__(self, value=None):
        if value is not None:
            self._value = value
