"""
The following classes are defined:
    ConditionCodeFlags
"""

from .. import wire
from .. import gate
from .. import signal
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
        enable: An object of type Wire. The enable input.
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

    Raises:
        TypeError: If data_bus is not a bus of width 16.
    """
    def __init__(self, d_bus, overflow, carry_out, enable, clock, z, v, n, c):
        if len(d_bus) != 16:
            raise TypeError(
                "Expected bus of width 16, received bus of width {0}.".format(
                    len(d_bus)
                )
            )

        vcc = Wire(1)

        z_not = Wire()
        v_not = Wire()
        n_not = Wire()
        c_not = Wire()

        z_mux_out = Wire()
        v_mux_out = Wire()
        n_mux_out = Wire()
        c_mux_out = Wire()

        or_1 = Wire()
        or_2 = Wire()
        or_3 = Wire()
        or_4 = Wire()
        or_5 = Wire()
        not_or = Wire()

        gate.ORGate4(*d_bus[0:4], or_1)
        gate.ORGate4(*d_bus[4:8], or_2)
        gate.ORGate4(*d_bus[8:12], or_3)
        gate.ORGate4(*d_bus[12:16], or_4)
        gate.ORGate4(or_1, or_2, or_3, or_4, or_5)
        gate.NOTGate(or_5, not_or)

        signal.Multiplexer2To1(vcc, enable, not_or, z, z_mux_out)
        signal.Multiplexer2To1(vcc, enable, overflow, v, v_mux_out)
        signal.Multiplexer2To1(vcc, enable, d_bus[0], n, n_mux_out)
        signal.Multiplexer2To1(vcc, enable, carry_out, c, c_mux_out)

        storage.DFlipFlop(z_mux_out, clock, z, z_not)
        storage.DFlipFlop(v_mux_out, clock, v, v_not)
        storage.DFlipFlop(n_mux_out, clock, n, n_not)
        storage.DFlipFlop(c_mux_out, clock, c, c_not)

        self.data_bus = d_bus
        self.overflow = overflow
        self.carry_out = carry_out
        self.enable = enable
        self.clock = clock
        self.z = z
        self.v = v
        self.n = n
        self.c = c

    def __str__(self):
        str_ = ""
        str_ += "data_bus: " + self.data_bus.__str__() + "\n"
        str_ += "overflow: " + str(self.overflow.value) + "\n"
        str_ += "carry_out: " + str(self.carry_out.value) + "\n"
        str_ += "enable: " + str(self.enable.value) + "\n"
        str_ += "clock: " + str(self.clock.value) + "\n"
        str_ += "z: " + str(self.z.value) + "\n"
        str_ += "v: " + str(self.v.value) + "\n"
        str_ += "n: " + str(self.n.value) + "\n"
        str_ += "c: " + str(self.c.value)
        return str_
