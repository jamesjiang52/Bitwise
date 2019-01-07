"""
The following classes are defined:
    Bus10
"""

from .. import wire

Wire = wire.Wire


class Bus10:
    """Initialize a new 10-bit bus.

    Args:
        wire_1, wire_2, ... , wire_10 (optional): Objects of type Wire. If not
            given, new wires will be created, which can then only be accessed
            by indexing the bus.

    Accessors:
        bus.wires: A tuple of the wires in the bus.
        bus.wire_values: A tuple of values of the wires in the bus.
    """
    def __init__(
        self,
        wire_1=None,
        wire_2=None,
        wire_3=None,
        wire_4=None,
        wire_5=None,
        wire_6=None,
        wire_7=None,
        wire_8=None,
        wire_9=None,
        wire_10=None
     ):
        if not wire_1:
            wire_1 = Wire()
        if not wire_2:
            wire_2 = Wire()
        if not wire_3:
            wire_3 = Wire()
        if not wire_4:
            wire_4 = Wire()
        if not wire_5:
            wire_5 = Wire()
        if not wire_6:
            wire_6 = Wire()
        if not wire_7:
            wire_7 = Wire()
        if not wire_8:
            wire_8 = Wire()
        if not wire_9:
            wire_9 = Wire()
        if not wire_10:
            wire_10 = Wire()

        self._wires = (
            wire_1,
            wire_2,
            wire_3,
            wire_4,
            wire_5,
            wire_6,
            wire_7,
            wire_8,
            wire_9,
            wire_10
        )

    @property
    def wires(self):
        return self._wires

    @property
    def wire_values(self):
        return tuple([wire.value for wire in self._wires])

    @wire_values.setter
    def wire_values(self, values):
        if len(values) != 10:
            raise TypeError(
                "Expected 10 arguments, received {0}.".format(len(values))
            )
        self._wires[0].value = values[0]
        self._wires[1].value = values[1]
        self._wires[2].value = values[2]
        self._wires[3].value = values[3]
        self._wires[4].value = values[4]
        self._wires[5].value = values[5]
        self._wires[6].value = values[6]
        self._wires[7].value = values[7]
        self._wires[8].value = values[8]
        self._wires[9].value = values[9]

    def __getitem__(self, key):
        if isinstance(key, slice):
            return tuple(
                self._wires[i] for i in range(*key.indices(len(self._wires)))
            )
        if (key >= 0) and (key < 10):
            return self._wires[key]
        else:
            raise IndexError("Bus width exceeded.")

    def __len__(self):
        return len(self._wires)

    def __str__(self):
        return str(self.wire_values)
