.. _haskus-system-build-steps:

Building steps
==============

This chapter explains how to compile and build a full system from scratch
*manually*. Using the ``haskus-system-build`` tool presented in
:ref:`haskus-system-build-tool` is much easier as it performs many of the
required steps presented in this chapter automatically.

We will follow these steps:

1. Building your own systems : these are user-space Linux ``init`` programs
   (written in Haskell and using the ``haskus-system`` framework) which are
   compiled statically.

2. Putting your systems into ramdisks.

3. Configuring and building the Linux kernel.
   
4. Testing with QEMU.

5. Configuring a boot-loader. Building ISO images and bootable devices. Testing
   the ISO image with QEMU.


.. include:: manual_building/building.rst
.. include:: manual_building/ramdisk.rst
.. include:: manual_building/linux.rst
.. include:: manual_building/QEMU.rst
.. include:: manual_building/distributing.rst
