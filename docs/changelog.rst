:tocdepth: 2


=========
Changelog
=========


Unreleased
==========

Added
-----
* Arithmetic-logic unit to processor subpackage
* Bitwise logic operations to logic subpackage
    * Bitwise AND, NAND, NOR, NOT, OR, XNOR, and XOR operations for 4-, 8-, and 16- bit inputs
* Multiplier classes to arithmetic subpackage
    * ``Multiplier2``
    * ``Multiplier4``
    * ``Multiplier8``

Changed
-------
* Added enable inputs to registers in ``storage.REG``


v0.2 - 2018-11-08
=================

Added
-----
* Tri-state buffer to wire subpackage
* Ring counter classes to state subpackage
    * ``RingCounter4``
    * ``RingCounter8``
    * ``RingCounter16``
    
* Up- and down-counter classes to state subpackage
    * ``UpCounterMod4``
    * ``UpCounterMod8``
    * ``UpCounterMod16``
    * ``DownCounterMod4``
    * ``DownCounterMod8``
    * ``DownCounterMod16``
    
* ``IMPLY`` logic gate to gate subpackage
* Shift register classes to state subpackage
    * ``ShiftRegister4``
    * ``ShiftRegister8``
    * ``ShiftRegister16``
    
* Parallel-to-serial converter classes to state subpackage
    * ``ParallelToSerialConverter4To1``
    * ``ParallelToSerialConverter8To1``
    * ``ParallelToSerialConverter16To1``
    
* Serial-to-parallel converter classes to state subpackage
    * ``SerialToParallelConverter1To4``
    * ``SerialToParallelConverter1To8``
    * ``SerialToParallelConverter1To16``
    
* Buffer class to gate subpackage

Changed
-------
* Added ``__getitem__()`` and ``__len__()`` methods to ``Bus4``, ``Bus8``, ``Bus16``, and ``BusSevenSegmentDisplay`` classes
* Rewrote docstrings for all existing classes
* Misc improvements


v0.1.1 - 2018-10-17
===================

Added
-----
* Storage register classes to storage subpackage
    * ``Register4``
    * ``Register8``
    * ``Register16``

Changed
-------
* Misc improvements
