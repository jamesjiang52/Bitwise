*******
Bitwise
*******

Bitwise is a Python library intended to make hardware design and simulation more accessible
for software engineers. While it can never replace a true hardware description language,
it aims to serve as a useful tool in the hardware design process, allowing a user to build and test
their digital circuits in a high-level programming language (i.e. with simpler syntax and more
flexible semantics) before implementing them using an HDL.

Getting Started
===============

Refer to the `documentation <https://bitwise.readthedocs.io/en/latest/>`_ for installation 
instructions, usage examples, API reference, and more.

Quick Example
=============

The following code creates a half-adder circuit::

    """
    Create a half-adder.
    """
    import bitwise as bw
    
    def main():
        # initialize inputs
        a = bw.wire.Wire()
        b = bw.wire.Wire()

        # initialize outputs
        sum_ = bw.wire.Wire()
        carry_out = bw.wire.Wire()

        # create circuit
        bw.gate.XORGate2(a, b, sum_)  # XORs a and b and puts the result into sum_
        bw.gate.ANDGate2(a, b, carry_out)  # ANDs a and b and puts the result into carry_out
        
    if __name__ == "__main__":
        main()

Interacting with it in a Python session::

    In [1]: a.value = 0
    
    In [2]: b.value = 0
    
    In [3]: sum.value
    Out[3]: 0
    
    In [4]: carry_out.value
    Out[4]: 0
    
    In [5]: a.value = 1
    
    In [6]: sum.value
    Out[6]: 1
    
    In [7]: carry_out.value
    Out[7]: 0
    
    In [8]: b.value = 1
    
    In [9]: sum.value
    Out[9]: 0
    
    In [10]: carry_out.value
    Out[10]: 1
