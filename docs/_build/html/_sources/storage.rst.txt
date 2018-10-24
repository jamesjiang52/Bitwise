:tocdepth: 2

=======
Storage
=======


.. _DFlipFlop:

DFlipFlop
=========

Class ``bw.storage.DFlipFlop``
------------------------------

.. image:: images/schematics/storage/DFlipFlop.svg
    :width: 360px

Defined in `bitwise/storage/FLOP.py <https://github.com/jamesjiang52/Bitwise/blob/master/bitwise/storage/FLOP.py>`_.

Positive edge-triggered `D flip-flop <https://en.wikipedia.org/wiki/Flip-flop_(electronics)#D_flip-flop>`_.

__init__
--------

::

    __init__(
        data,
        clock,
        output,
        output_not
    )

Construct a new positive edge-triggered D flip-flop.

Args:
~~~~~
* ``data``: An object of type ``Wire``. The data input to the flip-flop.
* ``clock``: An object of type ``Wire`` or ``Clock``. The clock input to the flip-flop.
* ``output``: An object of type ``Wire``. The output of the flip-flop. Takes on the value of ``data`` on the positive edges of ``clock``.
* ``output_not``: An object of type ``Wire``. The complemented form of ``output``.


.. _DFlipFlopPresetClear:

DFlipFlopPresetClear
====================

Class ``bw.storage.DFlipFlopPresetClear``
-----------------------------------------

.. image:: images/schematics/storage/DFlipFlopPresetClear.svg
    :width: 360px

Defined in `bitwise/storage/FLOP.py <https://github.com/jamesjiang52/Bitwise/blob/master/bitwise/storage/FLOP.py>`_.

Positive edge-triggered `D flip-flop <https://en.wikipedia.org/wiki/Flip-flop_(electronics)#D_flip-flop>`_ with asynchronous active low preset and clear.

__init__
--------

::

    __init__(
        data,
        preset_n,
        clear_n,
        clock,
        output,
        output_not
    )

Construct a new positive edge-triggered D flip-flop with preset/clear capabilities.

Args:
~~~~~
* ``data``: An object of type ``Wire``. The data input to the flip-flop.
* ``preset_n``: An object of type ``Wire``. Presets ``output`` to 1 and ``output_not`` to 0 if its value is 0.
* ``clear_n``: An object of type ``Wire``. Clears ``output`` to 0 and ``output_not`` to 1 if its value is 0.
* ``clock``: An object of type ``Wire`` or ``Clock``. The clock input to the flip-flop.
* ``output``: An object of type ``Wire``. The output of the flip-flop. Takes on the value of ``data`` on the positive edges of ``clock``.
* ``output_not``: An object of type ``Wire``. The complemented form of ``output``.


.. _GatedDLatch:

GatedDLatch
===========

Class ``bw.storage.GatedDLatch``
--------------------------------

.. image:: images/schematics/storage/GatedDLatch.svg
    :width: 360px

Defined in `bitwise/storage/FLOP.py <https://github.com/jamesjiang52/Bitwise/blob/master/bitwise/storage/FLOP.py>`_.

`Gated D latch <https://en.wikipedia.org/wiki/Flip-flop_(electronics)#Gated_D_latch>`_.

__init__
--------

::

    __init__(
        data,
        clock,
        output,
        output_not
    )

Construct a new gated D latch.

Args:
~~~~~
* ``data``: An object of type ``Wire``. The data input to the latch.
* ``clock``: An object of type ``Wire`` or ``Clock``. The clock input to the latch.
* ``output``: An object of type ``Wire``. The output of the latch. Takes on the value of ``data`` if the value of ``clock`` is 1.
* ``output_not``: An object of type ``Wire``. The complemented form of ``output``.


.. _GatedSRLatch:

GatedSRLatch
============

Class ``bw.storage.GatedSRLatch``
---------------------------------

.. image:: images/schematics/storage/GatedSRLatch.svg
    :width: 360px

Defined in `bitwise/storage/FLOP.py <https://github.com/jamesjiang52/Bitwise/blob/master/bitwise/storage/FLOP.py>`_.

`Gated SR latch <https://en.wikipedia.org/wiki/Flip-flop_(electronics)#Gated_SR_latch>`_.

__init__
--------

::

    __init__(
        set_,
        reset,
        clock,
        output,
        output_not
    )

Construct a new gated SR latch.

Args:
~~~~~
* ``set_``: An object of type ``Wire``. The set input to the latch.
* ``reset``: An object of type ``Wire``. The reset input to the latch.
* ``clock``: An object of type ``Wire`` or ``Clock``. The clock input to the latch.
* ``output``: An object of type ``Wire``. The output of the latch. When the value of ``clock`` is 1, takes on the value of 1 if the value of ``set`` is 1 and the value of 0 if the value of ``reset`` is 1.
* ``output_not``: An object of type ``Wire``. The complemented form of ``output``.


.. _JKFlipFlop:

JKFlipFlop
==========

Class ``bw.storage.JKFlipFlop``
-------------------------------

.. image:: images/schematics/storage/JKFlipFlop.svg
    :width: 360px

Defined in `bitwise/storage/FLOP.py <https://github.com/jamesjiang52/Bitwise/blob/master/bitwise/storage/FLOP.py>`_.

Positive edge-triggered `JK flip-flop <https://en.wikipedia.org/wiki/Flip-flop_(electronics)#JK_flip-flop>`_.

__init__
--------

::

    __init__(
        J,
        K,
        clock,
        output,
        output_not
    )

Construct a new positive edge-triggered JK flip-flop.

Args:
~~~~~
* ``J``: An object of type ``Wire``. The J input to the flip-flop.
* ``K``: An object of type ``Wire``. The K input to the flip-flop.
* ``clock``: An object of type ``Wire`` or ``Clock``. The clock input to the flip-flop.
* ``output``: An object of type ``Wire``. The output of the flip-flop. On the positive edges of ``clock``, takes on the value of 1 if the value of ``J`` is 1, takes on the value of 0 if the value of ``K`` is 1, and toggles its value if both ``J`` and ``K`` have value 1.
* ``output_not``: An object of type ``Wire``. The complemented form of ``output``.


.. _JKFlipFlopPresetClear:

JKFlipFlopPresetClear
=====================

Class ``bw.storage.JKFlipFlopPresetClear``
------------------------------------------

.. image:: images/schematics/storage/JKFlipFlopPresetClear.svg
    :width: 360px

Defined in `bitwise/storage/FLOP.py <https://github.com/jamesjiang52/Bitwise/blob/master/bitwise/storage/FLOP.py>`_.

Positive edge-triggered `JK flip-flop <https://en.wikipedia.org/wiki/Flip-flop_(electronics)#JK_flip-flop>`_ with asynchronous active low preset and clear.

__init__
--------

::

    __init__(
        J,
        K,
        preset_n,
        clear_n,
        clock,
        output,
        output_not
    )

Construct a new positive edge-triggered JK flip-flop with preset/clear capabilities.

Args:
~~~~~
* ``J``: An object of type ``Wire``. The J input to the flip-flop.
* ``K``: An object of type ``Wire``. The K input to the flip-flop.
* ``preset_n``: An object of type ``Wire``. Presets ``output`` to 1 and ``output_not`` to 0 if its value is 0.
* ``clear_n``: An object of type ``Wire``. Clears ``output`` to 0 and ``output_not`` to 1 if its value is 0.
* ``clock``: An object of type ``Wire`` or ``Clock``. The clock input to the flip-flop.
* ``output``: An object of type ``Wire``. The output of the flip-flop. On the positive edges of ``clock``, takes on the value of 1 if the value of ``J`` is 1, takes on the value of 0 if the value of ``K`` is 1, and toggles its value if both ``J`` and ``K`` have value 1.
* ``output_not``: An object of type ``Wire``. The complemented form of ``output``.


.. _ParallelToSerialConverter4To1:

ParallelToSerialConverter4To1
=============================

Class ``bw.signal.ParallelToSerialConverter4To1``
-------------------------------------------------

.. image:: images/schematics/storage/ParallelToSerialConverter4To1.svg
    :width: 600px

Defined in `bitwise/storage/PISO.py <https://github.com/jamesjiang52/Bitwise/blob/master/bitwise/storage/PISO.py>`_.

`4-bit-parallel-to-serial converter <https://en.wikipedia.org/wiki/Shift_register#Parallel-in_serial-out_(PISO)>`_.

__init__
--------

::

    __init__(
        enable,
        clear_n,
        load_n,
        data_bus,
        clock,
        output
    )

Construct a new 4-bit-parallel-to-serial converter.

Args:
~~~~~
* ``enable``: An object of type ``Wire``. Enables the converter.
* ``clear_n``: An object of type ``Wire``. Clears all 4 internal registers to 0 if its value is 0.
* ``load_n``: An object of type ``Wire``. The mode select. A value of 0 indicates a parallel load operation, where the values of ``data_bus`` are loaded into the internal registers. A value of 1 indicates a shift-right operation.
* ``data_bus``: An object of type ``Bus4``. The parallel data input.
* ``clock``: An object of type ``Wire`` or ``Clock``. The clock input.
* ``output``: An object of type ``Wire``. The serial output of the converter. ``data_bus[3]`` is outputted first, and ``data_bus[0]`` is outputted last.

Raises:
~~~~~~~
* ``TypeError``: If ``data_bus`` is not a bus of width 4.


.. _ParallelToSerialConverter8To1:

ParallelToSerialConverter8To1
=============================

Class ``bw.signal.ParallelToSerialConverter8To1``
-------------------------------------------------

.. image:: images/schematics/storage/ParallelToSerialConverter8To1.svg
    :width: 600px

Defined in `bitwise/storage/PISO.py <https://github.com/jamesjiang52/Bitwise/blob/master/bitwise/storage/PISO.py>`_.

`8-bit-parallel-to-serial converter <https://en.wikipedia.org/wiki/Shift_register#Parallel-in_serial-out_(PISO)>`_.

__init__
--------

::

    __init__(
        enable,
        clear_n,
        load_n,
        data_bus,
        clock,
        output
    )

Construct a new 8-bit-parallel-to-serial converter.

Args:
~~~~~
* ``enable``: An object of type ``Wire``. Enables the converter.
* ``clear_n``: An object of type ``Wire``. Clears all 8 internal registers to 0 if its value is 0.
* ``load_n``: An object of type ``Wire``. The mode select. A value of 0 indicates a parallel load operation, where the values of ``data_bus`` are loaded into the internal registers. A value of 1 indicates a shift-right operation.
* ``data_bus``: An object of type ``Bus8``. The parallel data input.
* ``clock``: An object of type ``Wire`` or ``Clock``. The clock input.
* ``output``: An object of type ``Wire``. The serial output of the converter. ``data_bus[7]`` is outputted first, and ``data_bus[0]`` is outputted last.

Raises:
~~~~~~~
* ``TypeError``: If ``data_bus`` is not a bus of width 8.


.. _ParallelToSerialConverter16To1:

ParallelToSerialConverter16To1
==============================

Class ``bw.signal.ParallelToSerialConverter16To1``
--------------------------------------------------

.. image:: images/schematics/storage/ParallelToSerialConverter16To1.svg
    :width: 600px

Defined in `bitwise/storage/PISO.py <https://github.com/jamesjiang52/Bitwise/blob/master/bitwise/storage/PISO.py>`_.

`16-bit-parallel-to-serial converter <https://en.wikipedia.org/wiki/Shift_register#Parallel-in_serial-out_(PISO)>`_.

__init__
--------

::

    __init__(
        enable,
        clear_n,
        load_n,
        data_bus,
        clock,
        output
    )

Construct a new 16-bit-parallel-to-serial converter.

Args:
~~~~~
* ``enable``: An object of type ``Wire``. Enables the converter.
* ``clear_n``: An object of type ``Wire``. Clears all 16 internal registers to 0 if its value is 0.
* ``load_n``: An object of type ``Wire``. The mode select. A value of 0 indicates a parallel load operation, where the values of ``data_bus`` are loaded into the internal registers. A value of 1 indicates a shift-right operation.
* ``data_bus``: An object of type ``Bus16``. The parallel data input.
* ``clock``: An object of type ``Wire`` or ``Clock``. The clock input.
* ``output``: An object of type ``Wire``. The serial output of the converter. ``data_bus[15]`` is outputted first, and ``data_bus[0]`` is outputted last.

Raises:
~~~~~~~
* ``TypeError``: If ``data_bus`` is not a bus of width 16.


.. _Register4:

Register4
=========

Class ``bw.storage.Register4``
------------------------------

.. image:: images/schematics/storage/Register4.svg
    :width: 800px

Defined in `bitwise/storage/REG.py <https://github.com/jamesjiang52/Bitwise/blob/master/bitwise/storage/REG.py>`_.

`4-bit storage register <https://en.wikipedia.org/wiki/Processor_register>`_.

__init__
--------

::

    __init__(
        data_bus,
        clock,
        output_bus
    )

Construct a new 4-bit storage register.

Args:
~~~~~
* ``data_bus``: An object of type ``Bus4``. The data input to the register.
* ``clock``: An object of type ``Wire`` or ``Clock``. The clock input to the register.
* ``output_bus``: An object of type ``Bus4``. The output of the register. Takes on the value of ``data_bus`` on the positive edges of ``clock``.

Raises:
~~~~~~~
* ``TypeError``: If either ``data_bus`` or ``output_bus`` is not a bus of width 4.


.. _Register8:

Register8
=========

Class ``bw.storage.Register8``
------------------------------

.. image:: images/schematics/storage/Register8.svg
    :width: 800px

Defined in `bitwise/storage/REG.py <https://github.com/jamesjiang52/Bitwise/blob/master/bitwise/storage/REG.py>`_.

`8-bit storage register <https://en.wikipedia.org/wiki/Processor_register>`_.

__init__
--------

::

    __init__(
        data_bus,
        clock,
        output_bus
    )

Construct a new 8-bit storage register.

Args:
~~~~~
* ``data_bus``: An object of type ``Bus8``. The data input to the register.
* ``clock``: An object of type ``Wire`` or ``Clock``. The clock input to the register.
* ``output_bus``: An object of type ``Bus8``. The output of the register. Takes on the value of ``data_bus`` on the positive edges of ``clock``.

Raises:
~~~~~~~
* ``TypeError``: If either ``data_bus`` or ``output_bus`` is not a bus of width 8.


.. _Register16:

Register16
==========

Class ``bw.storage.Register16``
-------------------------------

.. image:: images/schematics/storage/Register16.svg
    :width: 800px

Defined in `bitwise/storage/REG.py <https://github.com/jamesjiang52/Bitwise/blob/master/bitwise/storage/REG.py>`_.

`16-bit storage register <https://en.wikipedia.org/wiki/Processor_register>`_.

__init__
--------

::

    __init__(
        data_bus,
        clock,
        output_bus
    )

Construct a new 16-bit storage register.

Args:
~~~~~
* ``data_bus``: An object of type ``Bus16``. The data input to the register.
* ``clock``: An object of type ``Wire`` or ``Clock``. The clock input to the register.
* ``output_bus``: An object of type ``Bus16``. The output of the register. Takes on the value of ``data_bus`` on the positive edges of ``clock``.

Raises:
~~~~~~~
* ``TypeError``: If either ``data_bus`` or ``output_bus`` is not a bus of width 16.


.. _SerialToParallelConverter1To4:

SerialToParallelConverter1To4
=============================

Class ``bw.storage.SerialToParallelConverter1To4``
--------------------------------------------------

.. image:: images/schematics/storage/SerialToParallelConverter1To4.svg
    :width: 600px

Defined in `bitwise/storage/SIPO.py <https://github.com/jamesjiang52/Bitwise/blob/master/bitwise/storage/SIPO.py>`_.

`Serial-to-4-bit-parallel converter <https://en.wikipedia.org/wiki/Shift_register#Serial-in_parallel-out_(SIPO)>`_.

__init__
--------

::

    __init__(
        enable,
        clear_n,
        data,
        clock,
        output_bus
    )

Construct a new serial-to-4-bit-parallel converter.

Args:
~~~~~
* ``enable``: An object of type ``Wire``. Enables the converter.
* ``clear_n``: An object of type ``Wire``. Clears all 4 internal registers to 0 if its value is 0.
* ``data``: An object of type ``Wire``. The serial data input.
* ``clock``: An object of type ``Wire`` or ``Clock``. The clock input.
* ``output_bus``: An object of type ``Bus4``. The parallel output of the converter. ``output[0]`` corresponds to the most recent serial data input.

Raises:
~~~~~~~
* ``TypeError``: If ``output_bus`` is not a bus of width 4.


.. _SerialToParallelConverter1To8:

SerialToParallelConverter1To8
=============================

Class ``bw.storage.SerialToParallelConverter1To8``
--------------------------------------------------

.. image:: images/schematics/storage/SerialToParallelConverter1To8.svg
    :width: 600px

Defined in `bitwise/storage/SIPO.py <https://github.com/jamesjiang52/Bitwise/blob/master/bitwise/storage/SIPO.py>`_.

`Serial-to-8-bit-parallel converter <https://en.wikipedia.org/wiki/Shift_register#Serial-in_parallel-out_(SIPO)>`_.

__init__
--------

::

    __init__(
        enable,
        clear_n,
        data,
        clock,
        output_bus
    )

Construct a new serial-to-8-bit-parallel converter.

Args:
~~~~~
* ``enable``: An object of type ``Wire``. Enables the converter.
* ``clear_n``: An object of type ``Wire``. Clears all 8 internal registers to 0 if its value is 0.
* ``data``: An object of type ``Wire``. The serial data input.
* ``clock``: An object of type ``Wire`` or ``Clock``. The clock input.
* ``output_bus``: An object of type ``Bus8``. The parallel output of the converter. ``output[0]`` corresponds to the most recent serial data input.

Raises:
~~~~~~~
* ``TypeError``: If ``output_bus`` is not a bus of width 8.


.. _SerialToParallelConverter1To16:

SerialToParallelConverter1To16
==============================

Class ``bw.storage.SerialToParallelConverter1To16``
---------------------------------------------------

.. image:: images/schematics/storage/SerialToParallelConverter1To16.svg
    :width: 600px

Defined in `bitwise/storage/SIPO.py <https://github.com/jamesjiang52/Bitwise/blob/master/bitwise/storage/SIPO.py>`_.

`Serial-to-16-bit-parallel converter <https://en.wikipedia.org/wiki/Shift_register#Serial-in_parallel-out_(SIPO)>`_.

__init__
--------

::

    __init__(
        enable,
        clear_n,
        data,
        clock,
        output_bus
    )

Construct a new serial-to-16-bit-parallel converter.

Args:
~~~~~
* ``enable``: An object of type ``Wire``. Enables the converter.
* ``clear_n``: An object of type ``Wire``. Clears all 16 internal registers to 0 if its value is 0.
* ``data``: An object of type ``Wire``. The serial data input.
* ``clock``: An object of type ``Wire`` or ``Clock``. The clock input.
* ``output_bus``: An object of type ``Bus16``. The parallel output of the converter. ``output[0]`` corresponds to the most recent serial data input.

Raises:
~~~~~~~
* ``TypeError``: If ``output_bus`` is not a bus of width 16.


.. _ShiftRegister4:

ShiftRegister4
==============

Class ``bw.storage.ShiftRegister4``
-----------------------------------

.. image:: images/schematics/storage/ShiftRegister4.svg
    :width: 800px

Defined in `bitwise/storage/SHIFT.py <https://github.com/jamesjiang52/Bitwise/blob/master/bitwise/storage/SHIFT.py>`_.

`4-bit shift register <https://en.wikipedia.org/wiki/Shift_register>`_.

__init__
--------

::

    __init__(
        enable,
        clear_n,
        shift_load,
        data_bus,
        data_serial,
        clock,
        output_bus,
        output_serial
    )

Construct a new 4-bit shift register.

Args:
~~~~~
* ``enable``: An object of type ``Wire``. Enables the shift register.
* ``clear_n``: An object of type ``Wire``. Clears ``output_bus`` and ``output_serial`` to 0 if its value is 1.
* ``shift_load``: An object of type ``Wire``. The mode select. A value of 0 indicates a parallel load operation, where ``output_bus`` takes on the value of ``data_bus``. A value of 1 indicates a shift-right operation, where ``output_bus[3]`` takes on the value of ``output_bus[2]``, ``output_bus[2]`` takes on the value of ``output_bus[1]``, and so on; ``output_bus[0]`` takes on the value of ``data_serial``.
* ``data_bus``: An object of type ``Bus4``. The parallel data input.
* ``data_serial``. An object of type ``Wire``. The serial data input.
* ``clock``. An object of type ``Wire`` or ``Clock``. The clock input to the shift register.
* ``output_bus``. An object of type ``Bus4``. The parallel data output.
* ``output_serial``. An object of type ``Wire``. The serial data output. Identical to ``output_bus[3]``.

Raises:
~~~~~~~
* ``TypeError``: If either ``data_bus`` or ``output_bus`` is not a bus of width 4.


.. _ShiftRegister8:

ShiftRegister8
==============

Class ``bw.storage.ShiftRegister8``
-----------------------------------

.. image:: images/schematics/storage/ShiftRegister8.svg
    :width: 800px

Defined in `bitwise/storage/SHIFT.py <https://github.com/jamesjiang52/Bitwise/blob/master/bitwise/storage/SHIFT.py>`_.

`8-bit shift register <https://en.wikipedia.org/wiki/Shift_register>`_.

__init__
--------

::

    __init__(
        enable,
        clear_n,
        shift_load,
        data_bus,
        data_serial,
        clock,
        output_bus,
        output_serial
    )

Construct a new 8-bit shift register.

Args:
~~~~~
* ``enable``: An object of type ``Wire``. Enables the shift register.
* ``clear_n``: An object of type ``Wire``. Clears ``output_bus`` and ``output_serial`` to 0 if its value is 1.
* ``shift_load``: An object of type ``Wire``. The mode select. A value of 0 indicates a parallel load operation, where ``output_bus`` takes on the value of ``data_bus``. A value of 1 indicates a shift-right operation, where ``output_bus[7]`` takes on the value of ``output_bus[6]``, ``output_bus[6]`` takes on the value of ``output_bus[5]``, and so on; ``output_bus[0]`` takes on the value of ``data_serial``.
* ``data_bus``: An object of type ``Bus8``. The parallel data input.
* ``data_serial``. An object of type ``Wire``. The serial data input.
* ``clock``. An object of type ``Wire`` or ``Clock``. The clock input to the shift register.
* ``output_bus``. An object of type ``Bus8``. The parallel data output.
* ``output_serial``. An object of type ``Wire``. The serial data output. Identical to ``output_bus[7]``.

Raises:
~~~~~~~
* ``TypeError``: If either ``data_bus`` or ``output_bus`` is not a bus of width 8.


.. _ShiftRegister16:

ShiftRegister16
===============

Class ``bw.storage.ShiftRegister16``
------------------------------------

.. image:: images/schematics/storage/ShiftRegister16.svg
    :width: 800px

Defined in `bitwise/storage/SHIFT.py <https://github.com/jamesjiang52/Bitwise/blob/master/bitwise/storage/SHIFT.py>`_.

`16-bit shift register <https://en.wikipedia.org/wiki/Shift_register>`_.

__init__
--------

::

    __init__(
        enable,
        clear_n,
        shift_load,
        data_bus,
        data_serial,
        clock,
        output_bus,
        output_serial
    )

Construct a new 16-bit shift register.

Args:
~~~~~
* ``enable``: An object of type ``Wire``. Enables the shift register.
* ``clear_n``: An object of type ``Wire``. Clears ``output_bus`` and ``output_serial`` to 0 if its value is 1.
* ``shift_load``: An object of type ``Wire``. The mode select. A value of 0 indicates a parallel load operation, where ``output_bus`` takes on the value of ``data_bus``. A value of 1 indicates a shift-right operation, where ``output_bus[15]`` takes on the value of ``output_bus[14]``, ``output_bus[14]`` takes on the value of ``output_bus[13]``, and so on; ``output_bus[0]`` takes on the value of ``data_serial``.
* ``data_bus``: An object of type ``Bus16``. The parallel data input.
* ``data_serial``. An object of type ``Wire``. The serial data input.
* ``clock``. An object of type ``Wire`` or ``Clock``. The clock input to the shift register.
* ``output_bus``. An object of type ``Bus16``. The parallel data output.
* ``output_serial``. An object of type ``Wire``. The serial data output. Identical to ``output_bus[15]``.

Raises:
~~~~~~~
* ``TypeError``: If either ``data_bus`` or ``output_bus`` is not a bus of width 16.


.. _SRLatch:

SRLatch
=======

Class ``bw.storage.SRLatch``
----------------------------

.. image:: images/schematics/storage/SRLatch.svg
    :width: 360px

Defined in `bitwise/storage/FLOP.py <https://github.com/jamesjiang52/Bitwise/blob/master/bitwise/storage/FLOP.py>`_.

`SR latch <https://en.wikipedia.org/wiki/Flip-flop_(electronics)#Simple_set-reset_latches>`_.

__init__
--------

::

    __init__(
        set_,
        reset,
        output,
        output_not
    )

Construct a new SR latch.

Args:
~~~~~
* ``set_``: An object of type ``Wire``. The set input to the latch.
* ``reset``: An object of type ``Wire``. The reset input to the latch.
* ``output``: An object of type ``Wire``. The output of the latch. Takes on the value of 1 if the value of ``set`` is 1 and the value of 0 if the value of ``reset`` is 1.
* ``output_not``: An object of type ``Wire``. The complemented form of ``output``.


.. _TFlipFlop:

TFlipFlop
=========

Class ``bw.storage.TFlipFlop``
------------------------------

.. image:: images/schematics/storage/TFlipFlop.svg
    :width: 360px

Defined in `bitwise/storage/FLOP.py <https://github.com/jamesjiang52/Bitwise/blob/master/bitwise/storage/FLOP.py>`_.

Positive edge-triggered `T flip-flop <https://en.wikipedia.org/wiki/Flip-flop_(electronics)#T_flip-flop>`_.

__init__
--------

::

    __init__(
        toggle,
        clock,
        output,
        output_not
    )

Construct a new positive edge-triggered T flip-flop.

Args:
~~~~~
* ``toggle``: An object of type ``Wire``. The toggle input to the flip-flop.
* ``clock``: An object of type ``Wire`` or ``Clock``. The clock input to the flip-flop.
* ``output``: An object of type ``Wire``. The output of the flip-flop. Toggles its value on the positive edges of ``clock`` if the value of ``toggle`` is 1.
* ``output_not``: An object of type ``Wire``. The complemented form of ``output``.


.. _TFlipFlopPresetClear:

TFlipFlopPresetClear
====================

Class ``bw.storage.TFlipFlopPresetClear``
-----------------------------------------

.. image:: images/schematics/storage/TFlipFlopPresetClear.svg
    :width: 360px

Defined in `bitwise/storage/FLOP.py <https://github.com/jamesjiang52/Bitwise/blob/master/bitwise/storage/FLOP.py>`_.

Positive edge-triggered `T flip-flop <https://en.wikipedia.org/wiki/Flip-flop_(electronics)#T_flip-flop>`_ with asynchronous active low preset and clear.

__init__
--------

::

    __init__(
        toggle,
        preset_n,
        clear_n,
        clock,
        output,
        output_not
    )

Construct a new positive edge-triggered T flip-flop with preset/clear capabilities.

Args:
~~~~~
* ``toggle``: An object of type ``Wire``. The toggle input to the flip-flop.
* ``preset_n``: An object of type ``Wire``. Presets ``output`` to 1 and ``output_not`` to 0 if its value is 0.
* ``clear_n``: An object of type ``Wire``. Clears ``output`` to 0 and ``output_not`` to 1 if its value is 0.
* ``clock``: An object of type ``Wire`` or ``Clock``. The clock input to the flip-flop.
* ``output``: An object of type ``Wire``. The output of the flip-flop. Toggles its value on the positive edges of ``clock`` if the value of ``toggle`` is 1.
* ``output_not``: An object of type ``Wire``. The complemented form of ``output``.
