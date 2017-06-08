Building systems: the automated way (easy)
==========================================

Building and testing your own systems based on ``haskus-system`` requires quite
a fex steps: configuring and building a Linux kernel, etc.

Hopefully we have built a tool called ``haskus-system-build`` that performs all
these steps automatically. Read the next chapter if you want to understand the
details or do it manually.

You need to have `Stack <http://www.haskellstack.org>`_ installed.

Installing ``haskus-system-build``
----------------------------------

The tool is distributed with the ``haskus-system`` package.
To install the last version, use:

.. code::

   > git clone https://github.com/haskus/haskus-system.git
   > cd haskus-system
   > stack install

It will install it (and some other tools) into ``~/.local/bin``. Be sure to add
this path to your ``$PATH`` environment variable.

Getting started
---------------

To start a new project, enter a new directory and uses the ``init`` command:

.. code::

   > mkdir my-project
   > cd my-project
   > haskus-system-build init

It downloads the default system template into the current directory. It is
composed of 4 files:

.. code::

   > find . -type f
   ./stack.yaml
   ./src/Main.hs
   ./my-system.cabal
   ./system.yaml

* ``src/Main.hs`` is the system code

* ``my-system.cabal`` is the package configuration file that has been tweaked
  to build valid executables (use static linking, etc.)

* ``stack.yaml`` is Stack configuration file with the required dependencies on
  Haskus packages

* ``system.yaml`` is the ``haskus-system-build`` configuration file. Let's look
  into it:

.. code:: yaml

   linux:
      source: tarball
      version: 4.11.3
      options:
         enable:
            - CONFIG_DRM_BOCHS
            - CONFIG_DRM_RADEON
            - CONFIG_DRM_NOUVEAU
            - CONFIG_DRM_VIRTIO
         disable:
            - VIRTIO_BALLOON
      make-args: "-j8"
   
   ramdisk:
      init: my-system
   
   qemu:
      # Select a set of options for QEMU:
      #  "default": enable recommended options
      #  "vanilla": only use required settings to make tests work
      profile: vanilla
      options: ""
      kernel-args: ""

As you can see, it contains a Linux kernel configuration, a reference to our
system as being the ramdisk "init" program and some QEMU configuration.

Building and Testing
--------------------

You need to have some programs installed before we continue:

* everything required to build Linux: make, gcc, binutils...
* (un)packing tools: lzip, gzip, tar, cpio
* QEMU
* stack

Now let's try our system with QEMU!

.. code:: bash

   > haskus-system-build test

On the first execution, this command downloads and builds everything required to
test the system so it can take quite some time. Then QEMU's window should pop up
with our system running in it.

On following executions building is much faster because the tool reuses
previously built artefacts if the configuration hasn't changed.

Distributing and testing on real computers
------------------------------------------

If you want to distribute your system, the easiest way is to install it on an
**empty** storage device (e.g., usb stick).

**Warning: data on the device will be lost! Don't do that if you don't know what
you are doing!**

To install your system on the device whose device file is ``/dev/sde``:

.. code:: bash

   > haskus-system-build install --device /dev/sde

Note that you have to be in the *sudoers* list.


Another distribution method is to create an ISO image that you can distribute
online or burn on CD-ROMs.

.. code:: bash

   > haskus-system-build make-iso
   ...
   ISO built into .system-work/iso/my-system.iso

Note that you can test the ISO image with QEMU before you ship it:

.. code:: bash

   > haskus-system-build test-iso

This allows you to test the boot-loader configuration.
