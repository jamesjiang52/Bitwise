:tocdepth: 2

====
Wire
====


.. _Bus4:

Bus4
====

Class ``bw.wire.Bus4``
----------------------

Defined in `bitwise/wire/BUS.py <https://github.com/jamesjiang52/Bitwise/blob/master/bitwise/wire/BUS.py>`_.

4-bit bus.

__init__
--------

::

    __init__(
        wire_1,
        wire_2,
        wire_3,
        wire_4
    )

Initialize a new 4-bit bus.

Args:
~~~~~
* ``wire_1``, ``wire_2``, ... , ``wire_4``: Objects of type ``Wire``.

Accessors:
----------
* ``bus.wires``: A tuple of the wires in the bus.
* ``bus.wire_values``: A tuple of values of the wires in the bus.


.. _Bus8:

Bus8
====

Class ``bw.wire.Bus8``
----------------------

Defined in `bitwise/wire/BUS.py <https://github.com/jamesjiang52/Bitwise/blob/master/bitwise/wire/BUS.py>`_.

8-bit bus.

__init__
--------

::

    __init__(
        wire_1,
        wire_2,
        wire_3,
        wire_4,
        wire_5,
        wire_6,
        wire_7,
        wire_8
    )

Initialize a new 8-bit bus.

Args:
~~~~~
* ``wire_1``, ``wire_2``, ... , ``wire_8``: Objects of type ``Wire``.

Accessors:
----------
* ``bus.wires``: A tuple of the wires in the bus.
* ``bus.wire_values``: A tuple of values of the wires in the bus.


.. _Bus16:

Bus16
=====

Class ``bw.wire.Bus16``
-----------------------

Defined in `bitwise/wire/BUS.py <https://github.com/jamesjiang52/Bitwise/blob/master/bitwise/wire/BUS.py>`_.

16-bit bus.

__init__
--------

::

    __init__(
        wire_1,
        wire_2,
        wire_3,
        wire_4,
        wire_5,
        wire_6,
        wire_7,
        wire_8,
        wire_9,
        wire_10,
        wire_11,
        wire_12,
        wire_13,
        wire_14,
        wire_15,
        wire_16
    )

Initialize a new 16-bit bus.

Args:
~~~~~
* ``wire_1``, ``wire_2``, ... , ``wire_16``: Objects of type ``Wire``.

Accessors:
----------
* ``bus.wires``: A tuple of the wires in the bus.
* ``bus.wire_values``: A tuple of values of the wires in the bus.


.. _BusSevenSegmentDisplay:

BusSevenSegmentDisplay
======================

Class ``bw.wire.BusSevenSegmentDisplay``
----------------------------------------

Defined in `bitwise/wire/BUS.py <https://github.com/jamesjiang52/Bitwise/blob/master/bitwise/wire/BUS.py>`_.

Bus for output to a seven-segment display.

__init__
--------

::

    __init__(
        wire_1,
        wire_2,
        wire_3,
        wire_4,
        wire_5,
        wire_6,
        wire_7
    )

Initialize a new seven-segment display bus.

Args:
~~~~~
* ``wire_1``, ``wire_2``, ... , ``wire_7``: Objects of type ``Wire``.

Accessors:
----------
* ``bus.wires``: A tuple of the wires in the bus.
* ``bus.wire_values``: A tuple of values of the wires in the bus.


.. _TristateBuffer:

TristateBuffer
==============

Class ``bw.wire.TristateBuffer``
--------------------------------

Defined in `bitwise/wire/TRI.py <https://github.com/jamesjiang52/Bitwise/blob/master/bitwise/wire/TRI.py>`_.

`Tri-state buffer <https://en.wikipedia.org/wiki/Three-state_logic>`_.

__init__
--------

::

    __init__(
        enable,
        input_,
        output
    )

Initialize a new tri-state buffer.

Args:
~~~~~
* ``enable``: An object of type ``Wire``.
* ``input_``: An object of type ``Wire``.
* ``output``: An object of type ``Wire``. Takes on the value of ``input_`` if ``enable`` has value 1. Otherwise, value is independent of ``input_``.


.. _Wire:

Wire
====

Class ``bw.wire.Wire``
----------------------

Defined in `bitwise/wire/WIRE.py <https://github.com/jamesjiang52/Bitwise/blob/master/bitwise/wire/WIRE.py>`_.

A wire, with either a 0 or 1 integer value.

__init__
--------

::

    __init__()

Initialize a new wire with value 0.

After initialization, the value of the wire can be both accessed and mutated using ``wire.value``. For example::

    In [1]: import bitwise as bw
    
    In [2]: a = bw.wire.Wire()
    
    In [3]: a.value
    Out[3]: 0
    
    In [4]: a.value = 1
    
    In [5]: a.value
    Out[5]: 1

Raises:
~~~~~~~
* ``ValueError``: If value assigned to wire is not 0 or 1.
