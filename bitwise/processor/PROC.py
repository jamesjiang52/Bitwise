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
from . import BUS

Wire = wire.Wire
Clock = wire.Clock
Bus4 = wire.Bus4
Bus8 = wire.Bus8
Bus10 = BUS.Bus10
Bus16 = wire.Bus16
Buffer8 = wire.BufferBus8


# class Processor:
    """Construct a new 8-bit processor. It contains four 8-bit registers and
    communicates via an external 8-bit data bus.

    Operations are 10-bits wide. In cases where the number of bits that signify
    an operation is less than 10, only the most significant bits are used; the
    rest are ignored.

    The following is a list of op codes, where a, b, and c are 2-bit values
    representing registers:
        0000: No operation.
        0001 a b: Copy the value of register b into register a.
        0010 a #b: Move the immediate value of b into register a.
        0011 a b c: Add the value of register b and the value of register c and
            put the result into register a.
        0100 a b c: Subtract the value of register c from the value of register
            b and put the result into register a.
        0101 a b c: Multiply the value of register b and the value of register
            c and put the result (mod 256) into register a.
        0110 a b c: Bitwise AND the value of register b and the value of
            register c and put the result into register a.
        0111 a b c: Bitwise OR the value of register b and the value of
            register c and put the result into register a.
        1000 a b: Bitwise NOT the value of register b and put the result into
            register a.
        1001 a b c: Bitwise XOR the value of register b and the value of
            register c and put the result into register a.
        1010 a b c: Set the value of register a to 1 if the value of register b
            is greater than the value of register c. Otherwise, set register a
            to 0.
        1011 a b c: Set the value of register a to 1 if the value of register b
            is equal to the value of register c. Otherwise, set register a to
            0.
        1100 a b: Load the contents of the memory location given by register b
            into register a.
        1101 a b: Store the contents of register a into the memory location
            given by register b.
        1110: No operation.
        1111: No operation.
    """


# class _ProcessorDatapath:


# class _ProcessorControlpath:
