Building and Booting a System
=============================

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

**Automated method**

The `Build.hs
<http://github.com/haskus/haskus-system-examples/tree/master/src/Build.hs>`_
program in the `haskus-system-examples
<http://github.com/haskus/haskus-system-examples>`_ repository performs many of
the previous steps automatically. For instance, it downloads, configures and
installs ``Linux`` and ``Syslinux``; it builds ramdisks; it builds ``.iso``
images; it launches ``QEMU`` with appropriate options.

This is what we use at Haskus so it works at least on our setups. Its code
should be a good reference for now. In the future we may turn it into a proper
program distributed with the ``haskus-system`` to simplify its use.

.. include:: booting/building.rst
.. include:: booting/ramdisk.rst
.. include:: booting/linux.rst
.. include:: booting/QEMU.rst
.. include:: booting/distributing.rst
