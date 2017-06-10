Building systems: the manual way (harder)
=========================================

When a computer boots, the control is given to the following components in
sequence:

1. Hardware
2. BIOS (Basic input/output system)
3. Boot loader (e.g., Syslinux)
4. Kernel (e.g., Linux)
5. System (e.g., /init)

You first need to learn how to build your own systems : these are user-space
programs written in Haskell and using the ``haskus-system`` which are compiled
statically.

Then you need to build a Linux kernel and to put your systems into ramdisks.

Finally you can test your systems with ``QEMU`` and distribute them on physical
devices (USB sticks, CD-ROM) or online (as ``.iso`` images).

.. include:: manual_building/building.rst
.. include:: manual_building/ramdisk.rst
.. include:: manual_building/linux.rst
.. include:: manual_building/QEMU.rst
.. include:: manual_building/distributing.rst
