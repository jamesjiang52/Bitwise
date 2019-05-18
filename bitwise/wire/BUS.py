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
        wire_1, wire_2, ... , wire_4 (optional): Objects of type Wire. If not
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
        wire_4=None
    ):
        if not wire_1:
            wire_1 = Wire()
        if not wire_2:
            wire_2 = Wire()
        if not wire_3:
            wire_3 = Wire()
        if not wire_4:
            wire_4 = Wire()

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
        self._wires[0].value = values[0]
        self._wires[1].value = values[1]
        self._wires[2].value = values[2]
        self._wires[3].value = values[3]

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

    def __str__(self):
        return str(self.wire_values)

    def __call__(
        self,
        wire_1=None,
        wire_2=None,
        wire_3=None,
        wire_4=None
    ):
        if wire_1 is not None:
            self._wires[0].value = wire_1
        if wire_2 is not None:
            self._wires[1].value = wire_2
        if wire_3 is not None:
            self._wires[2].value = wire_3
        if wire_4 is not None:
            self._wires[3].value = wire_4


class Bus8:
    """Initialize a new 8-bit bus.

    Args:
        wire_1, wire_2, ... , wire_8 (optional): Objects of type Wire. If not
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
        wire_8=None
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
        self._wires[0].value = values[0]
        self._wires[1].value = values[1]
        self._wires[2].value = values[2]
        self._wires[3].value = values[3]
        self._wires[4].value = values[4]
        self._wires[5].value = values[5]
        self._wires[6].value = values[6]
        self._wires[7].value = values[7]

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

    def __str__(self):
        return str(self.wire_values)

    def __call__(
        self,
        wire_1=None,
        wire_2=None,
        wire_3=None,
        wire_4=None,
        wire_5=None,
        wire_6=None,
        wire_7=None,
        wire_8=None
    ):
        if wire_1 is not None:
            self._wires[0].value = wire_1
        if wire_2 is not None:
            self._wires[1].value = wire_2
        if wire_3 is not None:
            self._wires[2].value = wire_3
        if wire_4 is not None:
            self._wires[3].value = wire_4
        if wire_5 is not None:
            self._wires[4].value = wire_5
        if wire_6 is not None:
            self._wires[5].value = wire_6
        if wire_7 is not None:
            self._wires[6].value = wire_7
        if wire_8 is not None:
            self._wires[7].value = wire_8


class Bus16:
    """Initialize a new 16-bit bus.

    Args:
        wire_1, wire_2, ... , wire_16 (optional): Objects of type Wire. If not
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
        wire_10=None,
        wire_11=None,
        wire_12=None,
        wire_13=None,
        wire_14=None,
        wire_15=None,
        wire_16=None
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
        if not wire_11:
            wire_11 = Wire()
        if not wire_12:
            wire_12 = Wire()
        if not wire_13:
            wire_13 = Wire()
        if not wire_14:
            wire_14 = Wire()
        if not wire_15:
            wire_15 = Wire()
        if not wire_16:
            wire_16 = Wire()

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
        self._wires[10].value = values[10]
        self._wires[11].value = values[11]
        self._wires[12].value = values[12]
        self._wires[13].value = values[13]
        self._wires[14].value = values[14]
        self._wires[15].value = values[15]

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

    def __str__(self):
        return str(self.wire_values)

    def __call__(
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
        wire_10=None,
        wire_11=None,
        wire_12=None,
        wire_13=None,
        wire_14=None,
        wire_15=None,
        wire_16=None
    ):
        if wire_1 is not None:
            self._wires[0].value = wire_1
        if wire_2 is not None:
            self._wires[1].value = wire_2
        if wire_3 is not None:
            self._wires[2].value = wire_3
        if wire_4 is not None:
            self._wires[3].value = wire_4
        if wire_5 is not None:
            self._wires[4].value = wire_5
        if wire_6 is not None:
            self._wires[5].value = wire_6
        if wire_7 is not None:
            self._wires[6].value = wire_7
        if wire_8 is not None:
            self._wires[7].value = wire_8
        if wire_9 is not None:
            self._wires[8].value = wire_9
        if wire_10 is not None:
            self._wires[9].value = wire_10
        if wire_11 is not None:
            self._wires[10].value = wire_11
        if wire_12 is not None:
            self._wires[11].value = wire_12
        if wire_13 is not None:
            self._wires[12].value = wire_13
        if wire_14 is not None:
            self._wires[13].value = wire_14
        if wire_15 is not None:
            self._wires[14].value = wire_15
        if wire_16 is not None:
            self._wires[15].value = wire_16


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
        wire_1=None,
        wire_2=None,
        wire_3=None,
        wire_4=None,
        wire_5=None,
        wire_6=None,
        wire_7=None
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

    @wire_values.setter
    def wire_values(self, values):
        if len(values) != 7:
            raise TypeError(
                "Expected 7 arguments, received {0}.".format(len(values))
            )
        self._wires[0].value = values[0]
        self._wires[1].value = values[1]
        self._wires[2].value = values[2]
        self._wires[3].value = values[3]
        self._wires[4].value = values[4]
        self._wires[5].value = values[5]
        self._wires[6].value = values[6]
        self._wires[7].value = values[7]

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

    def __str__(self):
        return str(self.wire_values)

    def __call__(
        self,
        wire_1=None,
        wire_2=None,
        wire_3=None,
        wire_4=None,
        wire_5=None,
        wire_6=None,
        wire_7=None
    ):
        if wire_1 is not None:
            self._wires[0].value = wire_1
        if wire_2 is not None:
            self._wires[1].value = wire_2
        if wire_3 is not None:
            self._wires[2].value = wire_3
        if wire_4 is not None:
            self._wires[3].value = wire_4
        if wire_5 is not None:
            self._wires[4].value = wire_5
        if wire_6 is not None:
            self._wires[5].value = wire_6
        if wire_7 is not None:
            self._wires[6].value = wire_7
