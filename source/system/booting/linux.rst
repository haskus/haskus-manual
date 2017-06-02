Building the Linux kernel
-------------------------

The Linux kernel is required to execute systems using ``haskus-system``. Leaving
aside modules and firmwares, a compiled Linux kernel is a single binary file
that you can `use with QEMU </system/manual/booting/QEMU>`_ to execute your own
systems.

To build Linux, you first need to download it from `<http://kernel.org>`_ and to unpack
it:

.. code:: bash

   wget https://www.kernel.org/pub/linux/kernel/v4.x/linux-4.9.8.tar.xz
   tar xf linux-4.9.8.tar.xz

Then you need to configure it. We recommend at least the following:

.. code:: bash

   cd linux-4.9.8
   
   # default configuration for the X86-64 target
   make x86_64_defconfig

   # enable some DRM (graphics) drivers
   ./scripts/config -e CONFIG_DRM_BOCHS
   ./scripts/config -e CONFIG_DRM_RADEON
   ./scripts/config -e CONFIG_DRM_NOUVEAU
   ./scripts/config -e CONFIG_DRM_VIRTIO_GPU

   # fixup incoherent configuration (press enter when asked to select options)
   make oldconfig

If you know what you are doing, you can configure it further with:

.. code:: bash

   make xconfig

Finally, build the kernel with:

.. code:: bash

   make -j8

Copy the resulting kernel binary that you can `use with QEMU
</system/manual/booting/QEMU>`_ for instance:

.. code:: bash

   cp arch/x86/boot/bzImage linux-4.9.8.bin

**Automated method**

The `Build.hs
<http://github.com/haskus/haskus-system-examples/tree/master/src/Build.hs>`_
program in the `haskus-system-examples
<http://github.com/haskus/haskus-system-examples>`_ repository automatically
downloads, configures and builds the Linux kernel.
