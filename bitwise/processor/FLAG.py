"""
The following classes are defined:
    ConditionCodeFlags
"""

from .. import wire
from .. import gate
from .. import storage

Wire = wire.Wire
Bus4 = wire.Bus4
Bus8 = wire.Bus8
Bus16 = wire.Bus16


class ConditionCodeFlags:
    """Construct a new set of condition code flag flip-flops.

    Args:
        data_bus: An object of type Bus16. The data input to the flip-flops.
        overflow: An object of type Wire. The overflow input.
        carry_out: An object of type Wire. The carry-out input.
        clock: An object of type Wire or Clock. The clock input to the
            flip-flops.
        z: An object of type Wire. Indicates when the value on data_bus is
            equal to zero.
        v: An object of type Wire. Indicates when an arithmetic operation
            produces an overflow.
        n: An object of type Wire. Indicates when the value on data_bus is
            negative.
        c: An object of type Wire. Indicates when an arithmetic operation
            produces a carry-out.
    """
    def __init__(self, data_bus, overflow, carry_out, clock, z, v, n, c):
        if len(data_bus) != 16:
            raise TypeError(
                "Expected bus of width 16, received bus of width {0}.".format(
                    len(data_bus)
                )
            )

        z_not = Wire()
        v_not = Wire()
        n_not = Wire()
        c_not = Wire()

        or_1 = Wire()
        or_2 = Wire()
        or_3 = Wire()
        or_4 = Wire()
        or_5 = Wire()
        not_or = Wire()

        gate.ORGate4(*data_bus[0:4], or_1)
        gate.ORGate4(*data_bus[4:8], or_2)
        gate.ORGate4(*data_bus[8:12], or_3)
        gate.ORGate4(*data_bus[12:16], or_4)
        gate.ORGate4(or_1, or_2, or_3, or_4, or_5)
        gate.NOTGate(or_5, not_or)

        storage.DFlipFlop(not_or, clock, z, z_not)
        storage.DFlipFlop(overflow, clock, v, v_not)
        storage.DFlipFlop(data_bus[0], clock, n, n_not)
        storage.DFlipFlop(carry_out, clock, c, c_not)
