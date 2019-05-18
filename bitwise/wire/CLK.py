"""
The following classes are defined:
    Clock
"""

import _thread


class Clock:
    """
    This class simulates a clock, which allows for synchronous operations in
    logic circuits. It has a 50% duty cycle, oscillating between the values 0
    and 1.

    (As of now, this clock has no timer, and simply oscillates between 0 and 1
    as fast as Python and the OS allows.)
    """
    def __init__(self):
        self._value = 0
        self.connections = []

    @property
    def value(self):
        return self._value

    @value.setter
    def value(self, value):
        if ((value != 0) and (value != 1)):
            raise ValueError(
                "Wire value must be 0 or 1, received \"{0}\".".format(value))

        self._value = value
        for callback in self.connections:
            callback(self._value)

    def _bind_to(self, callback):
        self.connections.append(callback)

    def _oscillate(self):
        # no actual timer right now, will find out how to implement later
        while True:
            self.value = 0
            self.value = 1

    def start(self):
        _thread.start_new_thread(self._oscillate, ())

    def __str__(self):
        return str(self._value)

    def __call__(self, value=None):
        if value is not None:
            self._value = value
