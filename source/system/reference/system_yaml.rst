``system.yaml`` syntax
======================

This is the reference for ``system.yaml`` files used by ``haskus-system-build``
tool.

Linux kernel
------------

* ``linux.source``: how to retrieve the Linux kernel

   * ``tarball`` (default)
   * ``git`` (not yet implemented)

* ``linux.version``: which Linux version to use

   * requires ``linux.version=tarball``

* ``linux.options``:

   * ``enable``: list of Linux configuration options to enable
   * ``disable``: list of Linux configuration options to disable
   * ``module``: list of Linux configuration options to build as module

* ``linux.make-args``: string of arguments passed to ``make``

Ramdisk
-------

* ``ramdisk.init``: name of the program to use as init program


QEMU
----

* ``qemu.profile``: option profile to use

   * ``default`` (default): enable some recommended options
   * ``vanilla``: only set required options (-kernel, etc.)

* ``qemu.options``: string of additional arguments to pass to QEMU

* ``qemu.kernel-args``: string of additional arguments to pass to the Kernel

**Example**

.. code:: yaml

   linux:
      source: tarball
      version: 4.11.3
      options:
         enable:
            - CONFIG_DRM_BOCHS
            - CONFIG_DRM_VIRTIO
         disable:
            - CONFIG_DRM_RADEON
         module:
            - CONFIG_DRM_NOUVEAU
      make-args: "-j8"
   
   ramdisk:
      init: my-system
   
   qemu:
      profile: vanilla
      options: "-enable-kvm"
      kernel-args: "quiet"

