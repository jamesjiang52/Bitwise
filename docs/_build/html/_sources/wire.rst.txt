:tocdepth: 2

====
Wire
====


.. _BufferBus4:

BufferBus4
==========

Class ``bw.wire.BufferBus4``
----------------------------

Defined in `bitwise/wire/TRI_BUS.py <https://github.com/jamesjiang52/Bitwise/blob/master/bitwise/wire/TRI_BUS.py>`_.

4-bit `buffered <https://en.wikipedia.org/wiki/Three-state_logic>`_ bus.

__init__
--------

::

    __init__(
        enable,
        input_bus,
        output_bus
    )

Initialize a new tri-state buffer with buses of width 4 as input and output.

Args:
~~~~~
* ``enable``: An object of type ``Wire``.
* ``input_bus``: An object of type ``Bus4``.
* ``output_bus``: An object of type ``Bus4``. Takes on the value of ``input_bus`` if ``enable`` has value 1. Otherwise, value is independent of ``input_bus``.

Raises:
~~~~~~~
* ``TypeError``: If either ``input_bus`` or ``output_bus`` is not a bus of width 4.


.. _BufferBus8:

BufferBus8
==========

Class ``bw.wire.BufferBus8``
----------------------------

Defined in `bitwise/wire/TRI_BUS.py <https://github.com/jamesjiang52/Bitwise/blob/master/bitwise/wire/TRI_BUS.py>`_.

8-bit `buffered <https://en.wikipedia.org/wiki/Three-state_logic>`_ bus.

__init__
--------

::

    __init__(
        enable,
        input_bus,
        output_bus
    )

Initialize a new tri-state buffer with buses of width 8 as input and output.

Args:
~~~~~
* ``enable``: An object of type ``Wire``.
* ``input_bus``: An object of type ``Bus8``.
* ``output_bus``: An object of type ``Bus8``. Takes on the value of ``input_bus`` if ``enable`` has value 1. Otherwise, value is independent of ``input_bus``.

Raises:
~~~~~~~
* ``TypeError``: If either ``input_bus`` or ``output_bus`` is not a bus of width 8.


.. _BufferBus16:

BufferBus16
===========

Class ``bw.wire.BufferBus16``
-----------------------------

Defined in `bitwise/wire/TRI_BUS.py <https://github.com/jamesjiang52/Bitwise/blob/master/bitwise/wire/TRI_BUS.py>`_.

16-bit `buffered <https://en.wikipedia.org/wiki/Three-state_logic>`_ bus.

__init__
--------

::

    __init__(
        enable,
        input_bus,
        output_bus
    )

Initialize a new tri-state buffer with buses of width 16 as input and output.

Args:
~~~~~
* ``enable``: An object of type ``Wire``.
* ``input_bus``: An object of type ``Bus16``.
* ``output_bus``: An object of type ``Bus16``. Takes on the value of ``input_bus`` if ``enable`` has value 1. Otherwise, value is independent of ``input_bus``.

Raises:
~~~~~~~
* ``TypeError``: If either ``input_bus`` or ``output_bus`` is not a bus of width 16.


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
* ``wire_1``, ``wire_2``, ... , ``wire_4`` (optional): Objects of type ``Wire``. If not given, new wires will be created, which can then only be accessed by indexing the bus.

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
* ``wire_1``, ``wire_2``, ... , ``wire_8`` (optional): Objects of type ``Wire``. If not given, new wires will be created, which can then only be accessed by indexing the bus.

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
* ``wire_1``, ``wire_2``, ... , ``wire_16`` (optional): Objects of type ``Wire``. If not given, new wires will be created, which can then only be accessed by indexing the bus.

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
* ``wire_1``, ``wire_2``, ... , ``wire_7`` (optional): Objects of type ``Wire``. If not given, new wires will be created, which can then only be accessed by indexing the bus.

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
