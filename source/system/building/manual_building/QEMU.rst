Testing with QEMU
-----------------

To test a system with QEMU, we recommend that you first build a ramdisk
containing it, say ``myimage.img``. We suppose your system (i.e., the user-space
program) is stored in ``/my/system`` in the ramdisk.

You also need to build a recent Linux kernel, say ``linux.bin``.

To launch ``QEMU``, use the following command line:

.. code:: bash

   qemu-system-x86_64
      -kernel linux.bin
      -initrd myimage.img
      -append "rdinit=/my/system"

We recommend the following options for ``QEMU``:

.. code:: bash

   # make QEMU faster by using KVM
   -enable-kvm

   # use newer simulated hardware
   -machine q35
   
   # make pointer handling better by simulating a tablet
   -usbdevice "tablet"

   # redirect the guest Linux console on the host terminal
   -serial stdio
   -append "console=ttyS0"

   # enable better sound device
   -soundhw "hda"

   # make the guest Linux output more quiet
   -append "quiet"
