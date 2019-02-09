"""
The following classes are defined:
    Processor
"""

from .. import wire
from .. import gate
from .. import logic
from .. import signal
from .. import state
from .. import storage
from . import ALU

Wire = wire.Wire
Clock = wire.Clock
Bus4 = wire.Bus4
Bus8 = wire.Bus8
Bus16 = wire.Bus16


class Processor:
    """Construct a new 16-bit processor. It contains sixteen 16-bit registers
    and communicates via two external 16-bit data buses and an address bus.

    Instructions are 16-bits wide. In cases where the number of bits required
    to represent the instruction is less than 16, only the most significant
    bits are used; the rest are ignored.

    The following is a list of op codes, where a, b, and c are 4-bit values
    representing registers:
        0000: Halt execution. Internally, this is done by decrementing the
            program counter.
        0001 a b: Copy the value of register b into register a.
        0010 a #b: Store the immediate value of b in register a. b is taken as
            the 4th to 12th bits of the instruction.
        0011 a b c: Add the value of register b and the value of register c and
            store the result in register a.
        0100 a b c: Subtract the value of register c from the value of register
            b and store the result in register a.
        0101 a b c: Multiply the value of register b and the value of register
            c and store the result (mod 256) in register a.
        0110 a b c: Bitwise AND the value of register b and the value of
            register c and store the result in register a.
        0111 a b c: Bitwise OR the value of register b and the value of
            register c and store the result in register a.
        1000 a b: Bitwise NOT the value of register b and store the result in
            register a.
        1001 a: Branch to the memory location given in register a.
        1010 a b c: Set the value of register a to 1 if the value of register b
            is greater than the value of register c. Otherwise, set register a
            to 0.
        1011 a b c: Set the value of register a to 1 if the value of register b
            is equal to the value of register c. Otherwise, set register a to
            0.
        1100 a [b]: Load the contents of the memory location given in register
            b into register a.
        1101 a [b]: Store the contents of register a in the memory location
            given by register b.
        1110 a: Push the value of register a onto the stack. A stack overflow
            results in undefined behavior.
        1111 a: Pop the top value off the stack and store it in register a. An
            empty stack results in undefined behavior.
    """


# class _ProcessorDatapath:
# class _ProcessorControlpath:
