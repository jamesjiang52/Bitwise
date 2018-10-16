*******
Bitwise
*******

Bitwise is a Python library intended to make hardware design and simulation more accessible
for software engineers. While it can never replace a true hardware description language,
it aims to serve as a useful tool in the hardware design process, allowing a user to build and test
their digital circuits in a high-level programming language (i.e. with simpler syntax and more
flexible semantics) before implementing them using an HDL.

Quick Example
=============

The following code creates a half-adder circuit::

    import bitwise as bw
    
    # initialize inputs
    a = bw.wire.Wire()
    b = bw.wire.Wire()

    # initialize outputs
    sum = bw.wire.Wire()
    carry_out = bw.wire.Wire()

    # create circuit
    bw.gate.XORGate2(a, b, sum)  # XORs a and b and puts the result into sum
    bw.gate.ANDGate2(a, b, carry_out)  # ANDs a and b and puts the result into carry_out

Interacting with it in a Python session::

    >>> a.value = 0
    >>> b.value = 0
    >>> sum.value
    0
    >>> carry_out.value
    0
    >>> a.value = 1
    >>> sum.value
    1
    >>> carry_out.value
    0
    >>> b.value = 1
    >>> sum.value
    0
    >>> carry_out.value
    1
