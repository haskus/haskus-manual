=====================================================================
Distributing systems
=====================================================================

To distribute your systems, we will create a directory ``/my/disk`` containing:

* your system (in a ramdisk)
* the Linux kernel
* the boot-loader files (including its configuration)

A boot-loader is needed as it loads Linux and the ramdisk containing your
system. We use Syslinux boot-loader but you can use others such as GRUB. Note
that you don't need a boot-loader when you test your system with QEMU because
QEMU acts as a boot-loader itself.

To distribute your systems, you can install the boot-loader on a device (e.g.,
USB stick) and copy the files in the ``/my/disk`` directory on it. Or you can
also create a ``.iso`` image to burn on a CD-ROM (or to distribute online).


**Downloading Syslinux**

You first need to download and unpack the Syslinux boot-loader:

.. code:: bash

   wget http://www.kernel.org/pub/linux/utils/boot/syslinux/syslinux-6.03.tar.xz
   tar xf syslinux-6.03.tar.xz


**Creating the disk directory**

You need to execute the following steps to create your disk directory:

Create some directories:

.. code:: bash

   mkdir -p /my/disk/boot/syslinux

Copy Syslinux:

.. code:: bash

   find syslinux-6.03/bios *.c32 -exec cp {} /my/disk/boot/syslinux ;
   cp syslinux-6.03/bios/core/isolinux.bin /my/disk/boot/syslinux/

Copy the Linux kernel:

.. code:: bash

   cp linux-4.9.8.bin /my/disk/boot/

Copy the system ramdisk:

.. code:: bash

   cp myimage.img /my/disk/boot/

Finally, we need to configure the boot-loader by creating a file
``/my/disk/boot/syslinux/syslinux.cfg`` containing:

.. code::

   DEFAULT main
   PROMPT 0
   TIMEOUT 50
   UI vesamenu.c32
   
   LABEL main
   MENU LABEL MyOS
   LINUX  /boot/linux-4.9.8.bin
   INITRD /boot/myimage.img
   APPEND rdinit="/my/system"

Replace ``/my/system`` with the path of your system in the ``myimage.img``
ramdisk.


Creating a bootable device
--------------------------

To create a bootable device (e.g., bootable USB stick), you have to know its
device path (e.g., ``/dev/XXX``) and the partition that will contain the boot
files (e.g., ``/dev/XXX_N``).

You can use ``fdisk`` and ``mkfs.ext3`` to create an appropriate partition.

You have to install Syslinux MBR:

.. code:: bash

   sudo dd bs=440 if=syslinux-6.03/bios/mbr/mbr.bin of=/dev/XXX

Then you have to copy the contents of the disk directory on the partition and
configure it to be bootable:

.. code:: bash

   sudo mount /dev/XXX_N /mnt/SOMEWHERE
   sudo cp -rf /my/disk/* /mnt/SOMEWHERE
   sudo syslinux-6.03/bios/extlinux/extlinux --install /mnt/SOMEWHERE/boot/syslinux
   sudo umount /mnt/SOMEWHERE

Now your device should be bootable with your system!


Creating a bootable ISO
-----------------------

To create a bootable CD-ROM, you first need to create a ``.iso`` disk image with the ``xorriso`` utility:

.. code:: bash

   xorriso -as mkisofs
      -R -J                            # use Rock-Ridge/Joliet extensions
      -o mydisk.iso                    # output ISO file
      -c boot/syslinux/boot.cat        # create boot catalog
      -b boot/syslinux/isolinux.bin    # bootable binary file
      -no-emul-boot                    # does not use legacy floppy emulation
      -boot-info-table                 # write additional Boot Info Table (required by SysLinux)
      -boot-load-size 4
      -isohybrid-mbr syslinux-6.03/bios/mbr/isohdpfx_c.bin  # hybrid ISO
      /my/disk

It should create a ``mydisk.iso`` file that you can burn on a CD or distribute
online.
