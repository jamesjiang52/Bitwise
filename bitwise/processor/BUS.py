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
        wire_1=Wire(),
        wire_2=Wire(),
        wire_3=Wire(),
        wire_4=Wire(),
        wire_5=Wire(),
        wire_6=Wire(),
        wire_7=Wire(),
        wire_8=Wire(),
        wire_9=Wire(),
        wire_10=Wire()
     ):
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
        self.wires[0].value = values[0]
        self.wires[1].value = values[1]
        self.wires[2].value = values[2]
        self.wires[3].value = values[3]
        self.wires[4].value = values[4]
        self.wires[5].value = values[5]
        self.wires[6].value = values[6]
        self.wires[7].value = values[7]
        self.wires[8].value = values[8]
        self.wires[9].value = values[9]

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
