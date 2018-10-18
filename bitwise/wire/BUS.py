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
    def __init__(self, wire_1, wire_2, wire_3, wire_4):
        self._wires = (wire_1, wire_2, wire_3, wire_4)

    @property
    def wires(self):
        return self._wires

    @property
    def wire_values(self):
        return tuple([wire.value for wire in self._wires])
        
    def __getitem__(self, key):
        if (key >= 0) and (key < 4):
            return self._wires[key]
        elif (key < 0) and (key >= -4):
            return self._wires[4 + key]
        else:
            raise IndexError("Bus width exceeded.")


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
        wire_1,
        wire_2,
        wire_3,
        wire_4,
        wire_5,
        wire_6,
        wire_7,
        wire_8
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
        
    def __getitem__(self, key):
        if (key >= 0) and (key < 8):
            return self._wires[key]
        elif (key < 0) and (key >= -8):
            return self._wires[8 + key]
        else:
            raise IndexError("Bus width exceeded.")


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
        
    def __getitem__(self, key):
        if (key >= 0) and (key < 16):
            return self._wires[key]
        elif (key < 0) and (key >= -16):
            return self._wires[16 + key]
        else:
            raise IndexError("Bus width exceeded.")


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
        if (key >= 0) and (key < 7):
            return self._wires[key]
        elif (key < 0) and (key >= -7):
            return self._wires[7 + key]
        else:
            raise IndexError("Bus width exceeded.")
