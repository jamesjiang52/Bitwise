:tocdepth: 2

=========
Processor
=========


.. _ArithmeticLogicUnit:

ArithmeticLogicUnit
===================

Class ``bw.processor.ArithmeticLogicUnit``
------------------------------------------

.. image:: images/schematics/processor/ArithmeticLogicUnit.svg
    :width: 800px

Defined in `bitwise/processor/ALU.py <https://github.com/jamesjiang52/Bitwise/blob/master/bitwise/processor/ALU.py>`_.

16-bit `arithmetic logic unit <https://en.wikipedia.org/wiki/Arithmetic_logic_unit>`_, with functions defined below.

__init__
--------

::

    __init__(
        a_bus,
        b_bus,
        function_select_bus,
        overflow,
        carry_out,
        output_bus
    )

Construct a new 16-bit arithmetic-logic unit with the following functions:

::

        0000: a
        0001: NOT a
        0010: b
        0011: NOT b
        0100: a AND b
        0101: a NAND b
        0110: a OR b
        0111: a NOR b
        1000: a XOR b
        1001: a XNOR b
        1010: a PLUS b
        1011: NOT (a PLUS b)
        1100: a MINUS b
        1101: NOT (a MINUS b)
        1110: 0
        1111: 1

Args:
~~~~~
* ``a_bus``: An object of type ``Bus16``. The first input to the ALU. The first addend in add operations and the minuend in subtract operations. Also the number to be compared. ``a_bus[0]`` and ``a_bus[15]`` are the most and least significant bit, respectively.
* ``b_bus``: An object of type ``Bus16``. The second input to the ALU. The second addend in add operations and the subtrahend in subtract operations. Also the number to be compared against. ``b_bus[0]`` and ``b_bus[15]`` are the most and least significant bit, respectively.
* ``function_select_bus``: An object of type ``Bus4``. The function select input of the ALU, with functions defined above. ``function_select_bus[0]`` and ``function_select_bus[3]`` are the most and least significant bit, respectively.
* ``overflow``: An object of type ``Wire``. The arithmetic overflow indicator. Only valid for functions ``1100`` and ``1101`` (subtract operations).
* ``carry_out``: An object of type ``Wire``. The carry-out. Only valid for functions ``1010`` and ``1011`` (add operations).
* ``output_bus``: An object of type ``Bus16``. The output of the ALU. ``output_bus[0]`` and ``output_bus[15]`` are the most and least significant bit, respectively.

Raises:
~~~~~~~
* ``TypeError``: If either ``a_bus``, ``b_bus``, or ``output_bus`` is not a bus of width 16, or if ``fn_select_bus`` is not a bus of width 4.


.. _ConditionCodeFlags:

ConditionCodeFlags
==================

Class ``bw.processor.ConditionCodeFlags``
-----------------------------------------

.. image:: images/schematics/processor/ConditionCodeFlags.svg
    :width: 560px

Defined in `bitwise/processor/FLAG.py <https://github.com/jamesjiang52/Bitwise/blob/master/bitwise/processor/FLAG.py>`_.

`Condition code flag flip-flops <https://en.wikipedia.org/wiki/Status_register>`_.

__init__
--------

::

    __init__(
        data_bus, 
        overflow, 
        carry_out,
        enable,
        clock, 
        z, 
        v, 
        n, 
        c
    )

Construct a new set of condition code flag flip-flops.

Args:
~~~~~
* ``data_bus``: An object of type ``Bus16``. The data input to the flip-flops.
* ``overflow``: An object of type ``Wire``. The overflow input.
* ``carry_out``: An object of type ``Wire``. The carry-out input.
* ``enable``: An object of type ``Wire``. The enable input.
* ``clock``: An object of type ``Wire`` or ``Clock``. The clock input to the flip-flops.
* ``z``: An object of type ``Wire``. Indicates when the value on ``data_bus`` is equal to zero.
* ``v``: An object of type ``Wire``. Indicates when an arithmetic operation produces an overflow.
* ``n``: An object of type ``Wire``. Indicates when the value on ``data_bus`` is negative.
* ``c``: An object of type ``Wire``. Indicates when an arithmetic operation produces a carry-out.

Raises:
~~~~~~~
* ``TypeError``: If ``data_bus`` is not a bus of width 16.


.. _ProgramCounter:

ProgramCounter
==============

Class ``bw.processor.ProgramCounter``
-------------------------------------

.. image:: images/schematics/processor/ProgramCounter.svg
    :width: 800px

Defined in `bitwise/processor/PC.py <https://github.com/jamesjiang52/Bitwise/blob/master/bitwise/processor/PC.py>`_.

16-bit `program counter <https://en.wikipedia.org/wiki/Program_counter>`_.

__init__
--------

::

    __init__(
        data_bus, 
        up, 
        load, 
        clock, 
        output_bus
    )

Construct a new program counter with a 16-bit address space.

Args:
~~~~~
* ``data_bus``: An object of type ``Bus16``.
* ``up``: An object of type ``Wire``. If its value is 1, increments the program counter on the positive clock edge.
* ``load``: An object of type ``Wire``. If its value is 1, loads the value of ``data_bus`` into the program counter on the positive clock edge. If both up and load have value 1, ``load`` takes precedence.
* ``clock``: An object of type ``Wire`` or ``Clock``. The clock input.
* ``output_bus``: An object of type ``Bus16``. The address of the instruction to be executed.

Raises:
~~~~~~~
* ``TypeError``: If either ``data_bus`` or ``output_bus`` is not a bus of width 16.


.. _StackPointer:

StackPointer
============

Class ``bw.processor.StackPointer``
-----------------------------------

.. image:: images/schematics/processor/StackPointer.svg
    :width: 600px

Defined in `bitwise/processor/SP.py <https://github.com/jamesjiang52/Bitwise/blob/master/bitwise/processor/SP.py>`_.

16-bit `stack pointer <https://en.wikipedia.org/wiki/Stack_register>`_.

__init__
--------

::

    __init__(
        up, 
        down, 
        clock, 
        output_bus
    )

Construct a new stack pointer to a 16-bit address space.

Args:
~~~~~
* ``up``: An object of type ``Wire``. If its value is 1, increments the stack pointer on the positive clock edge.
* ``down``: An object of type ``Wire``. If its value is 1, decrements the stack pointer on the positive clock edge. If both ``up`` and ``down`` have value 1, ``down`` takes precedence.
* ``clock``: An object of type ``Wire`` or ``Clock``. The clock input.
* ``output_bus``: An object of type ``Bus16``. The address on top of the stack.

Raises:
~~~~~~~
* ``TypeError``: If ``output_bus`` is not a bus of width 16.
