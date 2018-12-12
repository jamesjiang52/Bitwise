"""
The following classes are defined:
    Bus4
    Bus8
    Bus16
    BusSevenSegmentDisplay
"""

from . import WIRE

Wire = WIRE.Wire


class Bus4:
    """Initialize a new 4-bit bus.

    Args:
        wire_1, wire_2, ... , wire_4: Objects of type Wire.

    Accessors:
        bus.wires: A tuple of the wires in the bus.
        bus.wire_values: A tuple of values of the wires in the bus.
    """
    def __init__(
        self,
        wire_1=Wire(),
        wire_2=Wire(),
        wire_3=Wire(),
        wire_4=Wire()
    ):
        self._wires = (wire_1, wire_2, wire_3, wire_4)

    @property
    def wires(self):
        return self._wires

    @property
    def wire_values(self):
        return tuple([wire.value for wire in self._wires])

    @wire_values.setter
    def wire_values(self, values):
        if len(values) != 4:
            raise TypeError(
                "Expected 4 arguments, received {0}.".format(len(values))
            )
        self.wires[0].value = values[0]
        self.wires[1].value = values[1]
        self.wires[2].value = values[2]
        self.wires[3].value = values[3]

    def __getitem__(self, key):
        if isinstance(key, slice):
            return tuple(
                self._wires[i] for i in range(*key.indices(len(self._wires)))
            )
        if (key >= 0) and (key < 4):
            return self._wires[key]
        else:
            raise IndexError("Bus width exceeded.")

    def __len__(self):
        return len(self._wires)


class Bus8:
    """Initialize a new 8-bit bus.

    Args:
        wire_1, wire_2, ... , wire_8: Objects of type Wire.

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
        wire_8=Wire()
     ):
        self._wires = (
            wire_1,
            wire_2,
            wire_3,
            wire_4,
            wire_5,
            wire_6,
            wire_7,
            wire_8
        )

    @property
    def wires(self):
        return self._wires

    @property
    def wire_values(self):
        return tuple([wire.value for wire in self._wires])

    @wire_values.setter
    def wire_values(self, values):
        if len(values) != 8:
            raise TypeError(
                "Expected 8 arguments, received {0}.".format(len(values))
            )
        self.wires[0].value = values[0]
        self.wires[1].value = values[1]
        self.wires[2].value = values[2]
        self.wires[3].value = values[3]
        self.wires[4].value = values[4]
        self.wires[5].value = values[5]
        self.wires[6].value = values[6]
        self.wires[7].value = values[7]

    def __getitem__(self, key):
        if isinstance(key, slice):
            return tuple(
                self._wires[i] for i in range(*key.indices(len(self._wires)))
            )
        if (key >= 0) and (key < 8):
            return self._wires[key]
        else:
            raise IndexError("Bus width exceeded.")

    def __len__(self):
        return len(self._wires)


class Bus16:
    """Initialize a new 16-bit bus.

    Args:
        wire_1, wire_2, ... , wire_16: Objects of type Wire.

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
        wire_10=Wire(),
        wire_11=Wire(),
        wire_12=Wire(),
        wire_13=Wire(),
        wire_14=Wire(),
        wire_15=Wire(),
        wire_16=Wire()
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
            wire_10,
            wire_11,
            wire_12,
            wire_13,
            wire_14,
            wire_15,
            wire_16
        )

    @property
    def wires(self):
        return self._wires

    @property
    def wire_values(self):
        return tuple([wire.value for wire in self._wires])

    @wire_values.setter
    def wire_values(self, values):
        if len(values) != 16:
            raise TypeError(
                "Expected 16 arguments, received {0}.".format(len(values))
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
        self.wires[10].value = values[10]
        self.wires[11].value = values[11]
        self.wires[12].value = values[12]
        self.wires[13].value = values[13]
        self.wires[14].value = values[14]
        self.wires[15].value = values[15]

    def __getitem__(self, key):
        if isinstance(key, slice):
            return tuple(
                self._wires[i] for i in range(*key.indices(len(self._wires)))
            )
        if (key >= 0) and (key < 16):
            return self._wires[key]
        else:
            raise IndexError("Bus width exceeded.")

    def __len__(self):
        return len(self._wires)


class BusSevenSegmentDisplay:
    """Initialize a new seven-segment display bus.

    Args:
        wire_1, wire_2, ... , wire_7: Objects of type Wire.

    Accessors:
        bus.wires: A tuple of the wires in the bus.
        bus.wire_values: A tuple of values of the wires in the bus.
    """
    def __init__(
        self,
        wire_1,
        wire_2,
        wire_3,
        wire_4,
        wire_5,
        wire_6,
        wire_7
     ):
        self._wires = (
            wire_1,
            wire_2,
            wire_3,
            wire_4,
            wire_5,
            wire_6,
            wire_7
        )

    @property
    def wires(self):
        return self._wires

    @property
    def wire_values(self):
        return tuple([wire.value for wire in self._wires])

    def __getitem__(self, key):
        if isinstance(key, slice):
            return tuple(
                self._wires[i] for i in range(*key.indices(len(self._wires)))
            )
        if (key >= 0) and (key < 7):
            return self._wires[key]
        else:
            raise IndexError("Bus width exceeded.")

    def __len__(self):
        return len(self._wires)
