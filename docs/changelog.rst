:tocdepth: 2

=========
Changelog
=========


Unreleased 
==========

Added
-----
* Shift register classes to storage subpackage
    * ``ShiftRegister4``
    * ``ShiftRegister8``
    * ``ShiftRegister16``
    
* Parallel-to-serial converter classes to signal subpackage
    * ``ParallelToSerialConverter4To1``
    * ``ParallelToSerialConverter8To1``
    * ``ParallelToSerialConverter16To1``
    
* Serial-to-parallel converter classes to signal subpackage
    * ``SerialToParallelConverter1To4``
    * ``SerialToParallelConverter1To8``
    * ``SerialToParallelConverter1To16``
    
* Buffer class to gate subpackage
* ``__getitem__()`` and ``__len__()`` methods to ``Bus4``, ``Bus8``, ``Bus16``, and ``BusSevenSegmentDisplay`` classes

Changed
-------
* Rewrote docstrings for all classes
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
