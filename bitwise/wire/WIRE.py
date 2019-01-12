"""
The following classes are defined:
    Wire
"""

import _thread


class Wire:
    """Initialize a new wire with value 0. After initialization, the value of
    the wire can be both accessed and mutated using wire.value.

    Raises:
        ValueError: If value assigned to wire is not 0 or 1.
    """
    def __init__(self, value=0):
        if ((value != 0) and (value != 1)):
            raise ValueError(
                "Wire value must be 0 or 1, received \"{0}\".".format(value))

        self._value = value
        self.connections = []

    @property
    def value(self):
        return self._value

    @value.setter
    def value(self, value):
        if ((value != 0) and (value != 1)):
            raise ValueError(
                "Wire value must be 0 or 1, received \"{0}\".".format(value))

        if value != self._value:
            self._value = value
            for callback in self.connections:
                # this line allows concurrency and should be used in real-world
                # applications
                # _thread.start_new_thread(callback, (self._value,))

                # this line stops all other execution and should be used for
                # testing purposes
                callback(self._value)

    def _bind_to(self, callback):
        if callback not in self.connections:
            self.connections.append(callback)

    def __str__(self):
        return str(self._value)
