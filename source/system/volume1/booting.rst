Building systems: the manual way (harder)
=========================================

When a computer boots, the control is given to the following components in
sequence:

1. Hardware
2. BIOS (Basic input/output system)
3. Boot loader (e.g., Syslinux)
4. Kernel (e.g., Linux)
5. System (e.g., /init)

You first need to learn how to `build your own systems
</system/manual/booting/building>`_: these are user-space programs written in
Haskell and using the ``haskus-system`` which are compiled statically.

Then you need to `build a Linux kernel </system/manual/booting/linux>`_ and to
`put your systems into ramdisks </system/manual/booting/ramdisk>`_.

Finally you can `test your systems </system/manual/booting/QEMU>`_ with ``QEMU``
and `distribute them </system/manual/booting/distributing>`_ on physical devices
(USB sticks, CD-ROM) or online (as ``.iso`` images).

.. include:: booting/building.rst
.. include:: booting/ramdisk.rst
.. include:: booting/linux.rst
.. include:: booting/QEMU.rst
.. include:: booting/distributing.rst
