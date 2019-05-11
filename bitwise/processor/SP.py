"""
The following classes are defined:
    StackPointer
"""

from .. import wire
from .. import arithmetic
from .. import gate
from .. import storage

Wire = wire.Wire
Bus4 = wire.Bus4
Bus8 = wire.Bus8
Bus16 = wire.Bus16


class StackPointer:
    """Construct a new stack pointer to a 16-bit address space.

    Args:
        up: An object of type Wire. If its value is 1, increments the stack
            pointer on the positive clock edge.
        down: An object of type Wire. If its value is 1, decrements the stack
            pointer on the positive clock edge. If both up and down have value
            1, down takes precedence.
        clock: An object of type Wire or Clock. The clock input.
        output_bus: An object of type Bus16. The address on top of the stack.

    Raises:
        TypeError: If output_bus is not a bus of width 16.
    """
    def __init__(self, up, down, clock, output_bus):
        if len(output_bus) != 16:
            raise TypeError(
                "Expected bus of width 16, received bus of width {0}.".format(
                    len(output_bus)
                )
            )

        b_bus = Bus16()
        b_bus.wires[15].value = 1
        sum_ = Bus16()

        v = Wire()
        c = Wire()
        enable = Wire()

        gate.ORGate2(up, down, enable)
        arithmetic.AdderSubtractor16(down, output_bus, b_bus, v, c, sum_)
        storage.Register16(sum_, enable, clock, output_bus)

        self.up = up
        self.down = down
        self.clock = clock
        self.output_bus = output_bus

    def __str__(self):
        str_ = ""
        str_ += "up: " + str(self.up.value) + "\n"
        str_ += "down: " + str(self.down.value) + "\n"
        str_ += "clock: " + str(self.clock.value) + "\n"
        str_ += "output_bus: " + self.output_bus.__str__()
        return str_

    def __call__(
        self, *,
        up=None,
        down=None,
        clock=None,
        output_bus=None
    ):
        if up is not None:
            self.up.value = up
        if down is not None:
            self.down.value = down
        if clock is not None:
            self.clock.value = clock
        if output_bus is not None:
            self.output_bus.wire_values = output_bus
