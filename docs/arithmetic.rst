:tocdepth: 2

==========
Arithmetic
==========


.. _Adder4:

Adder4
======

Class ``bw.arithmetic.Adder4``
------------------------------

.. image:: images/schematics/arithmetic/Adder4.svg
    :width: 800px

Defined in `bitwise/arithmetic/ADD.py <https://github.com/jamesjiang52/Bitwise/blob/master/bitwise/arithmetic/ADD.py>`_.

4-bit `adder <https://en.wikipedia.org/wiki/Adder_(electronics)>`_.

__init__
--------

::

    __init__(
        carry_in,
        a_bus,
        b_bus,
        carry_out,
        sum_bus
    )

Construct a new 4-bit adder.

Args:
~~~~~
* ``carry_in``: An object of type ``Wire``. The carry-in to the adder.
* ``a_bus``: An object of type ``Bus4``. The first addend. ``a_bus[0]`` and ``a_bus[3]`` are the most and least significant bit, respectively.
* ``b_bus``: An object of type ``Bus4``. The second addend. ``b_bus[0]`` and ``b_bus[3]`` are the most and least significant bit, respectively.
* ``carry_out``: An object of type ``Wire``. The carry-out of the adder.
* ``sum_bus``: An object of type ``Bus4``. The sum of the two addends. ``sum_bus[0]`` and ``sum_bus[3]`` are the most and least significant bit, respectively.

Raises:
~~~~~~~
* ``TypeError``: If either ``a_bus``, ``b_bus``, or ``sum_bus`` is not a bus of width 4.

.. highlight:: none

__str__
-------

Print out the wire values of the 4-bit adder. 

::

    carry_in: 0
    a_bus: (0, 0, 0, 0)
    b_bus: (0, 0, 0, 0)
    carry_out: 0
    sum_bus: (0, 0, 0, 0)

.. highlight:: python3
    
__call__
--------

::

    __call__(
        carry_in=None,
        a_bus=None,
        b_bus=None,
        carry_out=None,
        sum_bus=None
    )
    
Force specific values on the wires of the 4-bit adder.

Note that this method takes `zero` positional arguments; all values must be given as keyword arguments.


.. _Adder8:

Adder8
======

Class ``bw.arithmetic.Adder8``
------------------------------

.. image:: images/schematics/arithmetic/Adder8.svg
    :width: 800px

Defined in `bitwise/arithmetic/ADD.py <https://github.com/jamesjiang52/Bitwise/blob/master/bitwise/arithmetic/ADD.py>`_.

8-bit `adder <https://en.wikipedia.org/wiki/Adder_(electronics)>`_.

__init__
--------

::

    __init__(
        carry_in,
        a_bus,
        b_bus,
        carry_out,
        sum_bus
    )

Construct a new 8-bit adder.

Args:
~~~~~
* ``carry_in``: An object of type ``Wire``. The carry-in to the adder.
* ``a_bus``: An object of type ``Bus8``. The first addend. ``a_bus[0]`` and ``a_bus[7]`` are the most and least significant bit, respectively.
* ``b_bus``: An object of type ``Bus8``. The second addend. ``b_bus[0]`` and ``b_bus[7]`` are the most and least significant bit, respectively.
* ``carry_out``: An object of type ``Wire``. The carry-out of the adder.
* ``sum_bus``: An object of type ``Bus8``. The sum of the two addends. ``sum_bus[0]`` and ``sum_bus[7]`` are the most and least significant bit, respectively.

Raises:
~~~~~~~
* ``TypeError``: If either ``a_bus``, ``b_bus``, or ``sum_bus`` is not a bus of width 8.

.. highlight:: none

__str__
-------

Print out the wire values of the 8-bit adder. 

::

    carry_in: 0
    a_bus: (0, 0, 0, 0, 0, 0, 0, 0)
    b_bus: (0, 0, 0, 0, 0, 0, 0, 0)
    carry_out: 0
    sum_bus: (0, 0, 0, 0, 0, 0, 0, 0)

.. highlight:: python3
    
__call__
--------

::

    __call__(
        carry_in=None,
        a_bus=None,
        b_bus=None,
        carry_out=None,
        sum_bus=None
    )
    
Force specific values on the wires of the 8-bit adder.

Note that this method takes `zero` positional arguments; all values must be given as keyword arguments.


.. _Adder16:

Adder16
=======

Class ``bw.arithmetic.Adder16``
-------------------------------

.. image:: images/schematics/arithmetic/Adder16.svg
    :width: 800px

Defined in `bitwise/arithmetic/ADD.py <https://github.com/jamesjiang52/Bitwise/blob/master/bitwise/arithmetic/ADD.py>`_.

16-bit `adder <https://en.wikipedia.org/wiki/Adder_(electronics)>`_.

__init__
--------

::

    __init__(
        carry_in,
        a_bus,
        b_bus,
        carry_out,
        sum_bus
    )

Construct a new 16-bit adder.

Args:
~~~~~
* ``carry_in``: An object of type ``Wire``. The carry-in to the adder.
* ``a_bus``: An object of type ``Bus16``. The first addend. ``a_bus[0]`` and ``a_bus[15]`` are the most and least significant bit, respectively.
* ``b_bus``: An object of type ``Bus16``. The second addend. ``b_bus[0]`` and ``b_bus[15]`` are the most and least significant bit, respectively.
* ``carry_out``: An object of type ``Wire``. The carry-out of the adder.
* ``sum_bus``: An object of type ``Bus16``. The sum of the two addends. ``sum_bus[0]`` and ``sum_bus[15]`` are the most and least significant bit, respectively.

Raises:
~~~~~~~
* ``TypeError``: If either ``a_bus``, ``b_bus``, or ``sum_bus`` is not a bus of width 16.

.. highlight:: none

__str__
-------

Print out the wire values of the 16-bit adder. 

::

    carry_in: 0
    a_bus: (0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0)
    b_bus: (0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0)
    carry_out: 0
    sum_bus: (0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0)

.. highlight:: python3
    
__call__
--------

::

    __call__(
        carry_in=None,
        a_bus=None,
        b_bus=None,
        carry_out=None,
        sum_bus=None
    )
    
Force specific values on the wires of the 16-bit adder.

Note that this method takes `zero` positional arguments; all values must be given as keyword arguments.


.. _AdderSubtractor4:

AdderSubtractor4
================

Class ``bw.arithmetic.AdderSubtractor4``
----------------------------------------

.. image:: images/schematics/arithmetic/AdderSubtractor4.svg
    :width: 800px

Defined in `bitwise/arithmetic/ADD_SUB.py <https://github.com/jamesjiang52/Bitwise/blob/master/bitwise/arithmetic/ADD_SUB.py>`_.

4-bit `adder-subtractor <https://en.wikipedia.org/wiki/Adder%E2%80%93subtractor>`_.

__init__
--------

::

    __init__(
        add_subtract,
        a_bus,
        b_bus,
        overflow,
        carry_out,
        sum_bus
    )

Construct a new 4-bit adder-subtractor.

Args:
~~~~~
* ``add_subtract``: An object of type ``Wire``. Indicates the operation to carry out - 0 for addition, 1 for subtraction.
* ``a_bus``: An object of type ``Bus4``. The first addend, or the minuend. ``a_bus[0]`` and ``a_bus[3]`` are the most and least significant bit, respectively. ``a_bus[0]`` is the sign bit in subtraction operations.
* ``b_bus``: An object of type ``Bus4``. The second addend, or the subtrahend. ``b_bus[0]`` and ``b_bus[3]`` are the most and least significant bit, respectively. ``b_bus[0]`` is the sign bit in subtraction operations.
* ``overflow``: An object of type ``Wire``. The overflow indicator of the subtractor.
* ``carry_out``: An object of type ``Wire``. The carry-out of the adder.
* ``sum_bus``: An object of type ``Bus4``. The sum of the two addends, or the difference between the minuend and the subtrahend. ``sum_bus[0]`` and ``sum_bus[3]`` are the most and least significant bit, respectively. ``sum_bus[0]`` is the sign bit in subtraction operations.

Raises:
~~~~~~~
* ``TypeError``: If either ``a_bus``, ``b_bus``, or ``sum_bus`` is not a bus of width 4.

.. highlight:: none

__str__
-------

Print out the wire values of the 4-bit adder-subtractor. 

::

    add_subtract: 0
    a_bus: (0, 0, 0, 0)
    b_bus: (0, 0, 0, 0)
    overflow: 0
    carry_out: 0
    sum_bus: (0, 0, 0, 0)

.. highlight:: python3
    
__call__
--------

::

    __call__(
        add_subtract=None,
        a_bus=None,
        b_bus=None,
        overflow=None,
        carry_out=None,
        sum_bus=None
    )
    
Force specific values on the wires of the 4-bit adder-subtractor.

Note that this method takes `zero` positional arguments; all values must be given as keyword arguments.


.. _AdderSubtractor8:

AdderSubtractor8
================

Class ``bw.arithmetic.AdderSubtractor8``
----------------------------------------

.. image:: images/schematics/arithmetic/AdderSubtractor8.svg
    :width: 800px

Defined in `bitwise/arithmetic/ADD_SUB.py <https://github.com/jamesjiang52/Bitwise/blob/master/bitwise/arithmetic/ADD_SUB.py>`_.

8-bit `adder-subtractor <https://en.wikipedia.org/wiki/Adder%E2%80%93subtractor>`_.

__init__
--------

::

    __init__(
        add_subtract,
        a_bus,
        b_bus,
        overflow,
        carry_out,
        sum_bus
    )

Construct a new 8-bit adder-subtractor.

Args:
~~~~~
* ``add_subtract``: An object of type ``Wire``. Indicates the operation to carry out - 0 for addition, 1 for subtraction.
* ``a_bus``: An object of type ``Bus8``. The first addend, or the minuend. ``a_bus[0]`` and ``a_bus[7]`` are the most and least significant bit, respectively. ``a_bus[0]`` is the sign bit in subtraction operations.
* ``b_bus``: An object of type ``Bus8``. The second addend, or the subtrahend. ``b_bus[0]`` and ``b_bus[7]`` are the most and least significant bit, respectively. ``b_bus[0]`` is the sign bit in subtraction operations.
* ``overflow``: An object of type ``Wire``. The overflow indicator of the subtractor.
* ``carry_out``: An object of type ``Wire``. The carry-out of the adder.
* ``sum_bus``: An object of type ``Bus8``. The sum of the two addends, or the difference between the minuend and the subtrahend. ``sum_bus[0]`` and ``sum_bus[7]`` are the most and least significant bit, respectively. ``sum_bus[0]`` is the sign bit in subtraction operations.

Raises:
~~~~~~~
* ``TypeError``: If either ``a_bus``, ``b_bus``, or ``sum_bus`` is not a bus of width 8.

.. highlight:: none

__str__
-------

Print out the wire values of the 8-bit adder-subtractor. 

::

    add_subtract: 0
    a_bus: (0, 0, 0, 0, 0, 0, 0, 0)
    b_bus: (0, 0, 0, 0, 0, 0, 0, 0)
    overflow: 0
    carry_out: 0
    sum_bus: (0, 0, 0, 0, 0, 0, 0, 0)

.. highlight:: python3
    
__call__
--------

::

    __call__(
        add_subtract=None,
        a_bus=None,
        b_bus=None,
        overflow=None,
        carry_out=None,
        sum_bus=None
    )
    
Force specific values on the wires of the 8-bit adder-subtractor.

Note that this method takes `zero` positional arguments; all values must be given as keyword arguments.


.. _AdderSubtractor16:

AdderSubtractor16
=================

Class ``bw.arithmetic.AdderSubtractor16``
-----------------------------------------

.. image:: images/schematics/arithmetic/AdderSubtractor16.svg
    :width: 800px

Defined in `bitwise/arithmetic/ADD_SUB.py <https://github.com/jamesjiang52/Bitwise/blob/master/bitwise/arithmetic/ADD_SUB.py>`_.

16-bit `adder-subtractor <https://en.wikipedia.org/wiki/Adder%E2%80%93subtractor>`_.

__init__
--------

::

    __init__(
        add_subtract,
        a_bus,
        b_bus,
        overflow,
        carry_out,
        sum_bus
    )

Construct a new 16-bit adder-subtractor.

Args:
~~~~~
* ``add_subtract``: An object of type ``Wire``. Indicates the operation to carry out - 0 for addition, 1 for subtraction.
* ``a_bus``: An object of type ``Bus16``. The first addend, or the minuend. ``a_bus[0]`` and ``a_bus[15]`` are the most and least significant bit, respectively. ``a_bus[0]`` is the sign bit in subtraction operations.
* ``b_bus``: An object of type ``Bus16``. The second addend, or the subtrahend. ``b_bus[0]`` and ``b_bus[15]`` are the most and least significant bit, respectively. ``b_bus[0]`` is the sign bit in subtraction operations.
* ``overflow``: An object of type ``Wire``. The overflow indicator of the subtractor.
* ``carry_out``: An object of type ``Wire``. The carry-out of the adder.
* ``sum_bus``: An object of type ``Bus16``. The sum of the two addends, or the difference between the minuend and the subtrahend. ``sum_bus[0]`` and ``sum_bus[15]`` are the most and least significant bit, respectively. ``sum_bus[0]`` is the sign bit in subtraction operations.

Raises:
~~~~~~~
* ``TypeError``: If either ``a_bus``, ``b_bus``, or ``sum_bus`` is not a bus of width 16.

.. highlight:: none

__str__
-------

Print out the wire values of the 16-bit adder-subtractor. 

::

    add_subtract: 0
    a_bus: (0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0)
    b_bus: (0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0)
    overflow: 0
    carry_out: 0
    sum_bus: (0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0)

.. highlight:: python3
    
__call__
--------

::

    __call__(
        add_subtract=None,
        a_bus=None,
        b_bus=None,
        overflow=None,
        carry_out=None,
        sum_bus=None
    )
    
Force specific values on the wires of the 16-bit adder-subtractor.

Note that this method takes `zero` positional arguments; all values must be given as keyword arguments.


.. _FullAdder:

FullAdder
=========

Class ``bw.arithmetic.FullAdder``
---------------------------------

.. image:: images/schematics/arithmetic/FullAdder.svg
    :width: 400px

Defined in `bitwise/arithmetic/ADD.py <https://github.com/jamesjiang52/Bitwise/blob/master/bitwise/arithmetic/ADD.py>`_.

`Full adder <https://en.wikipedia.org/wiki/Adder_(electronics)#Full_adder>`_.

__init__
--------

::

    __init__(
        carry_in,
        a,
        b,
        carry_out,
        sum
    )

Construct a new full adder.

Args:
~~~~~
* ``carry_in``: An object of type ``Wire``. The carry-in to the adder.
* ``a``: An object of type ``Wire``. The first addend.
* ``b``: An object of type ``Wire``. The second addend.
* ``carry_out``: An object of type ``Wire``. The carry-out of the adder.
* ``sum``: An object of type ``Wire``. The sum of the two addends.

.. highlight:: none

__str__
-------

Print out the wire values of the full adder. 

::

    carry_in: 0
    a: 0
    b: 0
    carry_out: 0
    sum: 0

.. highlight:: python3
    
__call__
--------

::

    __call__(
        carry_in=None,
        a=None,
        b=None,
        carry_out=None,
        sum=None
    )
    
Force specific values on the wires of the full adder.

Note that this method takes `zero` positional arguments; all values must be given as keyword arguments.


.. _HalfAdder:

HalfAdder
=========

Class ``bw.arithmetic.HalfAdder``
---------------------------------

.. image:: images/schematics/arithmetic/HalfAdder.svg
    :width: 360px

Defined in `bitwise/arithmetic/ADD.py <https://github.com/jamesjiang52/Bitwise/blob/master/bitwise/arithmetic/ADD.py>`_.

`Half adder <https://en.wikipedia.org/wiki/Adder_(electronics)#Half_adder>`_.

__init__
--------

::

    __init__(
        a,
        b,
        carry_out,
        sum
    )

Construct a new half adder.

Args:
~~~~~
* ``a``: An object of type ``Wire``. The first addend.
* ``b``: An object of type ``Wire``. The second addend.
* ``carry_out``: An object of type ``Wire``. The carry-out of the adder.
* ``sum``: An object of type ``Wire``. The sum of the two addends.

.. highlight:: none

__str__
-------

Print out the wire values of the half adder. 

::

    a: 0
    b: 0
    carry_out: 0
    sum: 0

.. highlight:: python3
    
__call__
--------

::

    __call__(
        a=None,
        b=None,
        carry_out=None,
        sum=None
    )
    
Force specific values on the wires of the half adder.

Note that this method takes `zero` positional arguments; all values must be given as keyword arguments.


.. _Multiplier2:

Multiplier2
===========

Class ``bw.arithmetic.Multiplier2``
-----------------------------------

.. image:: images/schematics/arithmetic/Multiplier2.svg
    :width: 600px

Defined in `bitwise/arithmetic/MULT.py <https://github.com/jamesjiang52/Bitwise/blob/master/bitwise/arithmetic/MULT.py>`_.

2-bit unsigned array `multiplier <https://en.wikipedia.org/wiki/Binary_multiplier>`_.

__init__
--------

::

    __init__(
        a_1,
        a_2,
        b_1,
        b_2,
        product_bus
    )

Construct a new 2-bit unsigned multiplier.

Args:
~~~~~
* ``a_1``: An object of type ``Wire``. The most significant bit of the multiplicand.
* ``a_2``: An object of type ``Wire``. The least significant bit of the multiplicand.
* ``b_1``: An object of type ``Wire``. The most significant bit of the multiplier.
* ``b_2``: An object of type ``Wire``. The least significant bit of the multiplier.
* ``product_bus``: An object of type ``Bus4``. The product. ``product_bus[0]`` and ``product_bus[3]`` are the most and least significant bit, respectively.

Raises:
~~~~~~~
* ``TypeError``: If ``product_bus`` is not a bus of width 4.

.. highlight:: none

__str__
-------

Print out the wire values of the 2-bit unsigned multiplier. 

::

    a_1: 0
    a_2: 0
    b_1: 0
    b_2: 0
    product_bus: (0, 0, 0, 0)

.. highlight:: python3
    
__call__
--------

::

    __call__(
        a_1=None,
        a_2=None,
        b_1=None,
        b_2=None,
        product_bus=None
    )
    
Force specific values on the wires of the 2-bit unsigned multiplier.

Note that this method takes `zero` positional arguments; all values must be given as keyword arguments.


.. _Multiplier4:

Multiplier4
===========

Class ``bw.arithmetic.Multiplier4``
-----------------------------------

.. image:: images/schematics/arithmetic/Multiplier4.svg
    :width: 800px

Defined in `bitwise/arithmetic/MULT.py <https://github.com/jamesjiang52/Bitwise/blob/master/bitwise/arithmetic/MULT.py>`_.

4-bit unsigned array `multiplier <https://en.wikipedia.org/wiki/Binary_multiplier>`_.

__init__
--------

::

    __init__(
        a_bus,
        b_bus,
        product_bus
    )

Construct a new 4-bit unsigned multiplier.

Args:
~~~~~
* ``a_bus``: An object of type ``Bus4``. The multiplicand. ``a_bus[0]`` and ``a_bus[3]`` are the most and least significant bit, respectively.
* ``b_bus``: An object of type ``Bus4``. The multiplier. ``b_bus[0]`` and ``b_bus[3]`` are the most and least significant bit, respectively.
* ``product_bus``: An object of type ``Bus8``. The product. ``product_bus[0]`` and ``product_bus[7]`` are the most and least significant bit, respectively.

Raises:
~~~~~~~
* ``TypeError``: If either ``a_bus`` or ``b_bus`` is not a bus of width 4, or if ``product_bus`` is not a bus of width 8.

.. highlight:: none

__str__
-------

Print out the wire values of the 4-bit unsigned multiplier. 

::

    a_bus: (0, 0, 0, 0)
    b_bus: (0, 0, 0, 0)
    product_bus: (0, 0, 0, 0, 0, 0, 0, 0)

.. highlight:: python3
    
__call__
--------

::

    __call__(
        a_bus=None,
        b_bus=None,
        product_bus=None
    )
    
Force specific values on the wires of the 4-bit unsigned multiplier.

Note that this method takes `zero` positional arguments; all values must be given as keyword arguments.


.. _Multiplier8:

Multiplier8
===========

Class ``bw.arithmetic.Multiplier8``
-----------------------------------

.. image:: images/schematics/arithmetic/Multiplier8.svg
    :width: 800px

Defined in `bitwise/arithmetic/MULT.py <https://github.com/jamesjiang52/Bitwise/blob/master/bitwise/arithmetic/MULT.py>`_.

8-bit unsigned array `multiplier <https://en.wikipedia.org/wiki/Binary_multiplier>`_.

__init__
--------

::

    __init__(
        a_bus,
        b_bus,
        product_bus
    )

Construct a new 8-bit unsigned multiplier.

Args:
~~~~~
* ``a_bus``: An object of type ``Bus8``. The multiplicand. ``a_bus[0]`` and ``a_bus[7]`` are the most and least significant bit, respectively.
* ``b_bus``: An object of type ``Bus8``. The multiplier. ``b_bus[0]`` and ``b_bus[7]`` are the most and least significant bit, respectively.
* ``product_bus``: An object of type ``Bus16``. The product. ``product_bus[0]`` and ``product_bus[15]`` are the most and least significant bit, respectively.

Raises:
~~~~~~~
* ``TypeError``: If either ``a_bus`` or ``b_bus`` is not a bus of width 8, or if ``product_bus`` is not a bus of width 16.

.. highlight:: none

__str__
-------

Print out the wire values of the 16-bit unsigned multiplier. 

::

    a_bus: (0, 0, 0, 0, 0, 0, 0, 0)
    b_bus: (0, 0, 0, 0, 0, 0, 0, 0)
    product_bus: (0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0)

.. highlight:: python3
    
__call__
--------

::

    __call__(
        a_bus=None,
        b_bus=None,
        product_bus=None
    )
    
Force specific values on the wires of the 16-bit unsigned multiplier.

Note that this method takes `zero` positional arguments; all values must be given as keyword arguments.
