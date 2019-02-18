.. toctree::
    :hidden:
    
    api
    arithmetic
    changelog
    gate
    install
    logic
    processor
    signal
    state
    storage
    wire
    

=============
About Bitwise
=============

Bitwise is a Python library intended to make hardware design and simulation more accessible
for software engineers. While it can never replace a true hardware description language,
it aims to serve as a useful tool in the hardware design process, allowing a user to build and test
their digital circuits in a high-level programming language (i.e. with simpler syntax and more
flexible semantics) before implementing them using an HDL.

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

    >> a.value = 0
    >> b.value = 0
    >> sum_.value
    0
    >> carry_out.value
    0
    >> a.value = 1
    >> sum_.value
    1
    >> carry_out.value
    0
    >> b.value = 1
    >> sum_.value
    0
    >> carry_out.value
    1
