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

    In [1]: import bitwise as bw
    
    In [2]: bw.__version__
    Out[2]: '0.3'
    
Refer to the :doc:`changelog <changelog>` for version details. In this documentation, it is canonical to have imported Bitwise as ``bw``.


Basics
======

The use case of Bitwise is to model logic circuits in software; thus, it is useful to keep in mind that when you are using this library, you are actually `building a circuit` rather than writing a conventional software program.
To facilitate this, there are a plethora of new features that are introduced.

.. highlight:: python3

Wires
-----

By far the most important class defined by this library is ``Wire``. As the name suggests, ``Wire`` is used to connect different logic elements together, just like a real physical wire. They can be instantiated like so:: 

    In [1]: my_wire = bw.wire.Wire()
    
    In [2]: my_other_wire = bw.wire.Wire()


This creates two objects of type ``Wire``. Objects of this class have a single attribute, ``value``, that can take on 1 of two binary values (0 or 1). The default ``value`` is 0, and it can be both accessed and mutated 
using ``Wire.value``. ::

    In [3]: my_wire.value
    Out[3]: 0
    
    In [4]: my_other_wire.value
    Out[4]: 0
    
    In [5]: my_wire.value = 1
    
    In [6]: my_wire.value
    Out[6]: 1
    
    In [7]: my_wire.value = 0
    
    In [8]: my_wire.value
    Out[8]: 0
    
Other values will throw a ``ValueError``.

Buses are used to group together sets of 4, 8, and 16 wires, as well as 7 wires, meant for use with the ``SevenSegmentConverter`` modules. They are implemented using the ``Bus4``, ``Bus8``, ``Bus16``, and 
``BusSevenSegmentDisplay`` classes, respectively, and can be instantiated by specifying the appropriate number of ``Wire`` objects  ::

    In [1]: my_bus = bw.wire.Bus4(my_wire_1, my_wire_2, my_wire_3, my_wire_4)
    
or with no arguments, which will automatically create wires with default ``value`` 0. ::

    In [1]: my_bus = bw.wire.Bus4()
    
After instantiation, the ``Wire`` objects can be accessed using ``Bus.wires``, and the wire values can be both accessed and mutated using ``Bus.wire_values``. 


Logic Elements
--------------

Alongside ``Wire`` and ``Bus``, various logic elements are also at your disposal. These range from primitive logic gates to larger-scale logic circuits such as multiplexers and counters. In order to create new logic elements, usually a few
``Wire`` objects must be created first in order to make the necessary connections with the rest of your circuit. For example, the class that implements a two-input AND gate, ``ANDGate2``, takes three arguments in its 
``__init__`` method, all of type ``Wire``::

    In [1]: input_1 = bw.wire.Wire()
    
    In [2]: input_2 = bw.wire.Wire()
    
    In [3]: output = bw.wire.Wire()
    
    In [4]: my_AND_gate = bw.gate.ANDGate2(input_1, input_2, output)
    
Here, the value of ``output`` will take on the result of AND-ing the values of ``input_1`` and ``input_2``. Note that all the arguments must be present and valid for proper instantiation.
    
Logic elements that require buses as arguments may then be instantiated. For example, the 4-bit parity generator, ``ParityGenerator4``, receives two arguments: an object of type ``Bus4`` and an object of type ``Wire``. It
can be instantiated like so::

    In [1]: my_bus = bw.wire.Bus4()
    
    In [2]: output = bw.wire.Wire()
    
    In [3]: my_parity_generator = bw.logic.ParityGenerator4(my_bus, output)
    
For a catalog of all logic elements available, refer to the :doc:`API documentation <api>`.
    

Sensitivity
-----------

The concept of sensitivity is another key feature of this library. In a hardware circuit, when an input changes, the output changes immediately. In Bitwise, this type of behavior is accomplished by retaining a list of all 
the connections that an object of type ``Wire`` has, but it is akin to a sensitivity list in a true hardware description language like Verilog. To see sensitivity in action, consider again the ``ANDGate2`` example from the 
previous subsection::

    In [1]: input_1 = bw.wire.Wire()
    
    In [2]: input_2 = bw.wire.Wire()
    
    In [3]: output = bw.wire.Wire()
    
    In [4]: my_AND_gate = bw.gate.ANDGate2(input_1, input_2, output)
    
We can examine the value of ``output`` for every combination of values for ``input_1`` and ``input_2``. Recall that the default value of a ``Wire`` object is 0. ::

    In [5]: output.value
    Out[5]: 0
    
    In [6]: input_1.value = 1
    
    In [7]: output.value
    Out[7]: 0
    
    In [8]: input_1.value = 0
    
    In [9]: input_2.value = 1
    
    In [10]: output.value
    Out[10]: 0
    
    In [11]: input_1.value = 1
    
    In [12]: output.value
    Out[12]: 1
    
    In [13]: input_1.value = 0
    
    In [14]: input_2.value = 0
    
    In [15]: output.value
    Out[15]: 0
    
Notice that ``output.value`` reacts immediately to changes in the values of ``input_1`` and ``input_2``, mimicking the behavior of a hardware circuit. (Moreover, we've verified that the two-input AND gate works as intended.)

Hierarchy
---------

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

Objects of ``MyLogicElement`` can now be instantiated::

    In [1]: input_1 = bw.wire.Wire()
    
    In [2]: input_2 = bw.wire.Wire()
    
    In [3]: input_3 = bw.wire.Wire()
    
    In [4]: output = bw.wire.Wire()
    
    In [5]: my_logic_element = MyLogicElement(input_1, input_2, input_3, output)
    
We can test various values for ``input_1``, ``input_2``, and ``input_3`` to verify that the circuit works as intended::

    In [6]: output.value
    Out[6]: 0
    
    In [7]: input_1.value = 1
    
    In [8]: output.value
    Out[8]: 0
    
    In [9]: input_2.value = 1
    
    In [10]: output.value
    Out[10]: 1
    
    In [11]: input_1.value = 0
    
    In [12]: input_2.value = 0
    
    In [13]: output.value
    Out[13]: 0
    
    In [14]: input_3.value = 1
    
    In [15]: output.value
    Out[15]: 1
    
Additionally, objects of ``MyLogicElement`` can now be instantiated in other logic elements and circuits.


Example
-------

As a short example, let us construct a 2-bit adder. An adder simply takes two inputs, of a certain width, and outputs the sum. In this case, since we have two inputs of width 2, four inputs to the adder are needed.
Additionally, three outputs are needed, since the sum of two 2-bit numbers can be at most 3 bits wide.

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
            
Here, ``sum_1`` and ``sum_2`` are the most and least significant bits of the sum, respectively. It can be verified that this element behaves as intended::

    In [1]: carry_in = bw.wire.Wire()
    
    In [2]: input_1 = bw.wire.Wire()
    
    In [3]: input_2 = bw.wire.Wire()
    
    In [4]: sum_1 = bw.wire.Wire()
    
    In [5]: sum_2 = bw.wire.Wire()
    
    In [6]: myOneBitAdder = OneBitAdder(carry_in, input_1, input_2, sum_1, sum_2)
    
    In [7]: sum_1.value
    Out[7]: 0
    
    In [8]: sum_2.value
    Out[8]: 0
    
    In [9]: input_1.value = 1
    
    In [10]: sum_1.value
    Out[10]: 0
    
    In [11]: sum_2.value
    Out[11]: 1
    
    In [12]: input_2.value = 1
    
    In [13]: sum_1.value
    Out[13]: 1
    
    In [14]: sum_2.value
    Out[14]: 0
    
    In [15]: carry_in.value = 1
    
    In [16]: sum_1.value
    Out[16]: 1
    
    In [17]: sum_2.value
    Out[17]: 1
    
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

Again, it can be verified that this element behaves as intended by trying out a few test cases::

    In [1]: i_1_a = bw.wire.Wire()
    
    In [2]: i_1_b = bw.wire.Wire()
    
    In [3]: i_2_a = bw.wire.Wire()
    
    In [4]: i_2_b = bw.wire.Wire()
    
    In [5]: sum_1 = bw.wire.Wire()
    
    In [6]: sum_2 = bw.wire.Wire()
    
    In [7]: sum_3 = bw.wire.Wire()
    
    In [8]: myTwoBitAdder = TwoBitAdder(i_1_a, i_1_b, i_2_a, i_2_b, sum_1, sum_2, sum_3)
    
    In [9]: sum_1.value
    Out[9]: 0
    
    In [10]: sum_2.value
    Out[10]: 0
    
    In [11]: sum_3.value
    Out[11]: 0
    
    In [12]: i_1_b.value = 1
    
    In [13]: i_2_b.value = 1
    
    In [14]: sum_1.value
    Out[14]: 0
    
    In [15]: sum_2.value
    Out[15]: 1
    
    In [16]: sum_3.value
    Out[16]: 0
    
    In [17]: i_1_a.value = 1
    
    In [18]: sum_1.value
    Out[18]: 1
    
    In [19]: sum_2.value
    Out[19]: 0
    
    In [20]: sum_3.value
    Out[20]: 0
    
    In [21]: i_2_a.value = 1
    
    In [22]: sum_1.value
    Out[22]: 1
    
    In [23]: sum_2.value
    Out[23]: 1
    
    In [24]: sum_3.value
    Out[24]: 0
    
All of the sums are as expected.


Issues
======

Please post all bugs, issues, and feature requests in the `issues <https://github.com/jamesjiang52/Bitwise/issues>`_ section of the Github repository.
