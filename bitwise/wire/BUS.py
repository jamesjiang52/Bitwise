"""
This module defines classes that simulate buses, which are simply groups of
wires that serve as common input or output.

The following classes are defined:
    Bus4
    Bus8
    Bus16
    BusSevenSegmentDisplay
"""


class Bus4:
    """
    This bus is four bits wide:

        wire_1 |----------------|
        wire_2 |----------------|
        wire_3 |----------------|
        wire_4 |----------------|

    """
    def __init__(self, wire_1, wire_2, wire_3, wire_4):
        self._wires = (wire_1, wire_2, wire_3, wire_4)

    @property
    def wires(self):
        return self._wires

    @property
    def wire_values(self):
        return tuple([wire.value for wire in self._wires])


class Bus8:
    """
    This bus is eight bits wide:

        wire_1 |----------------|
        wire_2 |----------------|
        wire_3 |----------------|
        wire_4 |----------------|
        wire_5 |----------------|
        wire_6 |----------------|
        wire_7 |----------------|
        wire_8 |----------------|

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


class Bus16:
    """
    This bus is sixteen bits wide:

         wire_1 |----------------|
         wire_2 |----------------|
         wire_3 |----------------|
         wire_4 |----------------|
         wire_5 |----------------|
         wire_6 |----------------|
         wire_7 |----------------|
         wire_8 |----------------|
         wire_9 |----------------|
        wire_10 |----------------|
        wire_11 |----------------|
        wire_12 |----------------|
        wire_13 |----------------|
        wire_14 |----------------|
        wire_15 |----------------|
        wire_16 |----------------|

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


class BusSevenSegmentDisplay:
    """
    This bus is seven bits wide, and is specially used for seven-segment
    displays:

         wire_1 |----------------|
         wire_2 |----------------|
         wire_3 |----------------|
         wire_4 |----------------|
         wire_5 |----------------|
         wire_6 |----------------|
         wire_7 |----------------|

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
