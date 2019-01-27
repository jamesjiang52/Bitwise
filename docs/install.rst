:tocdepth: 3
.. highlight:: none

===============
Getting Started
===============


Installation
============

Before installing, make sure your Python version is 3.5 or greater. Older versions may work properly but are unsupported. To check your Python version, use ::
    
    $ python --version
    
in a terminal or command prompt.

If this requirement is not met, you can download the latest version of Python `here <https://www.python.org/downloads/>`_.

Once Python is installed, you can install Bitwise using ::

    $ pip install bitwise
    
If there are no errors, you can open a Python session and verify the installation by checking the version number::

    >> import bitwise as bw
    >> bw.__version__
    '0.3'
    
Refer to the :doc:`changelog <changelog>` for version details. In this documentation, it is canonical to have imported Bitwise as ``bw``.


Basics
======

The use case of Bitwise is to model logic circuits in software; thus, it is useful to keep in mind that when you are using this library, you are actually `building a circuit` rather than writing a conventional software program.
To facilitate this, there are a plethora of new features that are introduced.


Wires
-----

By far the most important class defined by this library is ``Wire``. As the name suggests, ``Wire`` is used to connect different logic elements together, just like a real physical wire. They can be instantiated like so:: 

    >> my_wire = bw.wire.Wire()
    >> my_other_wire = bw.wire.Wire()


This creates two objects of type ``Wire``. Objects of this class have a single attribute, ``value``, that can take on 1 of two binary values (0 or 1). The default ``value`` is 0, and it can be both accessed and mutated 
using ``Wire.value``. ::

    >> my_wire.value
    0
    >> my_other_wire.value
    0
    >> my_wire.value = 1
    >> my_wire.value
    1
    >> my_wire.value = 0
    >> my_wire.value
    0
    
Other values will throw a ``ValueError``.


Logic Elements
--------------

Alongside ``Wire``, various logic elements are also at your disposal. These range from primitive logic gates to larger-scale logic circuits such as multiplexers and counters. In order to create new logic elements, usually a few
``Wire`` objects must be created first in order to make the necessary connections with the rest of your circuit. For example, the class that implements a two-input AND gate, ``ANDGate2``, takes three arguments in its 
``__init__`` method, all of type ``Wire``::

    >> input_1 = bw.wire.Wire()
    >> input_2 = bw.wire.Wire()
    >> output = bw.wire.Wire()
    >> my_AND_gate = bw.gate.ANDGate2(input_1, input_2, output)
    
Here, the value of ``output`` will take on the result of AND-ing the values of ``input_1`` and ``input_2``. Note that all the arguments must be present and valid for proper instantiation.

Some logic elements may require buses as arguments in order to be created. In Bitwise, buses are simply groups of objects of type ``Wire``, and can be instantiated simply by giving the appropriate bus class the
correct number of arguments. For example, the following creates a bus of width 4::

    >> wire_1 = bw.wire.Wire()
    >> wire_2 = bw.wire.Wire()
    >> wire_3 = bw.wire.Wire()
    >> wire_4 = bw.wire.Wire()
    >> my_bus = bw.wire.Bus4(wire_1, wire_2, wire_3, wire_4)
    
After instantiation, the wires can be accessed using ``Bus.wires[index]``, or more simply by ``Bus[index]``. The values of the wires can also be accessed using ``Bus.wire_values[index]``. Note that in order to *mutate* the
values of the wires in the bus, the value of the individual ``Wire`` must be mutated. For example::

    >> my_bus.wire_values
    (0, 0, 0, 0)
    >> wire_2.value = 1
    >> my_bus.wire_values
    (0, 1, 0, 0)
    >> wire_2.value
    1
    >> my_bus.wire_values[1]
    1
    >> my_bus.wires[1].value
    1
    >> my_bus[1].value
    1
    
Logic elements that require buses as arguments may then be instantiated. For example, the 4-bit parity generator, ``ParityGenerator4``, receives two arguments: an object of type ``Bus4`` and an object of type ``Wire``. It
can be instantiated like so::

    >> output = bw.wire.Wire()
    >> my_parity_generator = bw.logic.ParityGenerator4(my_bus, output)
    
For a catalog of all logic elements available, refer to the :doc:`API documentation <api>`.
    

Sensitivity
-----------

The concept of sensitivity is another key feature of this library. In a hardware circuit, when an input changes, the output changes immediately. In Bitwise, this type of behavior is accomplished by retaining a list of all 
the connections that an object of type ``Wire`` has, but it is akin to a sensitivity list in a true hardware description language like Verilog. To see sensitivity in action, consider again the ``ANDGate2`` example from the 
previous subsection::

    >> input_1 = bw.wire.Wire()
    >> input_2 = bw.wire.Wire()
    >> output = bw.wire.Wire()
    >> my_AND_gate = bw.gate.ANDGate2(input_1, input_2, output)
    
We can examine the value of ``output`` for every combination of values for ``input_1`` and ``input_2``. Recall that the default value of a ``Wire`` object is 0. ::

    >> output.value
    0
    >> input_1.value = 1
    >> output.value
    0
    >> input_1.value = 0
    >> input_2.value = 1
    >> output.value
    0
    >> input_1.value = 1
    >> output.value
    1
    >> input_1.value = 0
    >> input_2.value = 0
    >> output.value
    0
    
Notice that ``output.value`` reacts immediately to changes in the values of ``input_1`` and ``input_2``, mimicking the behavior of a hardware circuit. (Moreover, we've verified that the two-input AND gate works as intended.)

Hierarchy
---------

.. highlight:: python3

In order to build higher-level logic circuits, the concept of hierarchy must be introduced. Quite simply, logic elements can have instances of other logic elements, which in turn can have instances of yet other logic elements.
The result is a hierarchical design pattern, with the primitive logic gates at the bottom (since they do not instantiate any other elements). For example, consider the following Python script, which defines a logic element 
with three inputs and one output. The value of ``output`` is the result of AND'ing the first two inputs and OR'ing the result with the third input. ::

    import bitwise as bw
    
    class MyLogicElement:
        def __init__(self, input_1, input_2, input_3, output):
            wire_1 = bw.wire.Wire()  # used as the output of the AND gate
            bw.gate.ANDGate2(input_1, input_2, wire_1)
            bw.gate.ORGate2(wire_1, input_3, output)
            
Notice, first and foremost, that the class has only one method, ``__init__``, which only takes in arguments of type ``Wire`` (and bus types) and whose purpose is simply to make the necessary wire connections with the rest 
of the logic circuit. Notice also that the logic element itself instantiates an object of type ``Wire``, ``wire_1``. This is necessary in order to internally connect the output of the two-input AND gate to one of the inputs 
of the OR gate. Lastly, notice that both the inputs and the output of the logic element are given as arguments to the ``__init__`` method. Again, this is necessary so that both the inputs and the output are connected in 
some way to the rest of the logic circuit.

.. highlight:: none

Objects of ``MyLogicElement`` can now be instantiated::

    >> input_1 = bw.wire.Wire()
    >> input_2 = bw.wire.Wire()
    >> input_3 = bw.wire.Wire()
    >> output = bw.wire.Wire()
    >> my_logic_element = MyLogicElement(input_1, input_2, input_3, output)
    
We can test various values for ``input_1``, ``input_2``, and ``input_3`` to verify that the circuit works as intended::

    >> output.value
    0
    >> input_1.value = 1
    >> output.value
    0
    >> input_2.value = 1
    >> output.value
    1
    >> input_1.value = 0
    >> input_2.value = 0
    >> output.value
    0
    >> input_3.value = 1
    >> output.value
    1
    
Additionally, objects of ``MyLogicElement`` can now be instantiated in other logic elements and circuits.


Example
-------

As a short example, let us construct a 2-bit adder. An adder simply takes two inputs, of a certain width, and outputs the sum. In this case, since we have two inputs of width 2, four inputs to the adder are needed.
Additionally, three outputs are needed, since the sum of two 2-bit numbers can be at most 3 bits wide.

.. highlight:: python3

Before a full 2-bit adder can be constructed, we first need a 1-bit adder. This adder must have not two, but three inputs, since we need one input to be a "carry-in" input from the previous 1-bit adder. Two outputs
are also needed. Skipping a few details, the following script defines a class that simulates our 1-bit adder::

    import bitwise as bw
    
    class OneBitAdder:
        def __init__(self, carry_in, input_1, input_2, sum_1, sum_2):
            # these wires connect the appropriate gates together (trust me, it works)
            wire_1 = bw.wireWire()
            wire_2 = bw.wire.Wire()
            wire_3 = bw.wire.Wire()

            bw.gate.XORGate2(input_1, input_2, wire_1)
            bw.gate.XORGate2(carry_in, wire_1, sum_2)
            bw.gate.ANDGate2(input_1, input_2, wire_2)
            bw.gate.ANDGate2(carry_in, wire_1, wire_3)
            bw.gate.ORGate2(wire_2, wire_3, sum_1)
            
.. highlight:: none
            
Here, ``sum_1`` and ``sum_2`` are the most and least significant bits of the sum, respectively. It can be verified that this element behaves as intended::

    >>> carry_in = bw.wire.Wire()
    >>> input_1 = bw.wire.Wire()
    >>> input_2 = bw.wire.Wire()
    >>> sum_1 = bw.wire.Wire()
    >>> sum_2 = bw.wire.Wire()
    >>> myOneBitAdder = OneBitAdder(carry_in, input_1, input_2, sum_1, sum_2)
    >>> sum_1.value
    0
    >>> sum_2.value
    0
    >>> input_1.value = 1
    >>> sum_1.value
    0
    >>> sum_2.value
    1
    >>> input_2.value = 1
    >>> sum_1.value
    1
    >>> sum_2.value
    0
    >>> carry_in.value = 1
    >>> sum_1.value
    1
    >>> sum_2.value
    1
    
.. highlight:: python3
    
A 2-bit adder may now be constructed by creating two instances of ``OneBitAdder`` in a hierarchical design pattern and connecting the wires appropriately::

    class TwoBitAdder:
        def __init__(self, input_1_a, input_1_b, input_2_a, input_2_b, sum_1, sum_2, sum_3):
            wire_1 = bw.wire.Wire()  # used to connect the two 1-bit adders
            gnd = bw.wire.Wire()
            gnd.value = 0
            
            OneBitAdder(gnd, input_1_b, input_2_b, wire_1, sum_3)
            OneBitAdder(wire_1, input_1_a, input_2_a, sum_1, sum_2)
            
Here, ``input_1_a`` and ``input_1_b`` are the most and least significant bits of the first input, respectively, ``input_2_a`` and ``input_2_b`` are the most and least significant bits of the second input, respectively, and 
``sum_1`` and ``sum_3`` are the most and least significant bits of the sum, respectively. Since the least significant adder has no carry-in, the ``gnd`` wire is used for the ``carry_in`` input. The ``wire_1`` wire is used to
connect the most significant bit of the sum from the least significant adder with the carry-in of the most significant adder.

.. highlight:: none

Again, it can be verified that this element behaves as intended by trying out a few test cases::

    >>> i_1_a = bw.wire.Wire()
    >>> i_1_b = bw.wire.Wire()
    >>> i_2_a = bw.wire.Wire()
    >>> i_2_b = bw.wire.Wire()
    >>> sum_1 = bw.wire.Wire()
    >>> sum_2 = bw.wire.Wire()
    >>> sum_3 = bw.wire.Wire()
    >>> myTwoBitAdder = TwoBitAdder(i_1_a, i_1_b, i_2_a, i_2_b, sum_1, sum_2, sum_3)
    >>> sum_1.value
    0
    >>> sum_2.value
    0
    >>> sum_3.value
    0
    >>> i_1_b.value = 1
    >>> i_2_b.value = 1
    >>> sum_1.value
    0
    >>> sum_2.value
    1
    >>> sum_3.value
    0
    >>> i_1_a.value = 1
    >>> sum_1.value
    1
    >>> sum_2.value
    0
    >>> sum_3.value
    0
    >>> i_2_a.value = 1
    >>> sum_1.value
    1
    >>> sum_2.value
    1
    >>> sum_3.value
    0
    
All of the sums are as expected (00 + 00 = 000, 01 + 01 = 010, 11 + 01 = 100, 11 + 11 = 110).


Issues
======

Please post all bugs, issues, and feature requests in the `issues <https://github.com/jamesjiang52/Bitwise/issues>`_ section of the Github repository.
