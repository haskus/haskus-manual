.. _system-tut-building:

==============================================================================
Building, testing and distributing systems
==============================================================================

In this tutorial we guide you into installing the required tools to start using
``haskus-system`` to create your own systems, to test them within QEMU and to
distribute them.

------------------------------------------------------------------------------
Installing dependencies
------------------------------------------------------------------------------

You need to install several programs and libraries before you can start
using ``haskus-system``. Most of them are very common and are required to build
a Linux kernel.

Please install the following packages:

* git
* `Stack <http://www.haskellstack.org>`_
* make
* gcc
* binutils
* static libraries (e.g., glibc-static and zlib-static packages on Fedora)
* (un)packing tools: lzip, gzip, tar, cpio
* QEMU

------------------------------------------------------------------------------
Installing haskus-system-build tool
------------------------------------------------------------------------------

To get started, you need to install the ``haskus-system-build`` program. It is
available on Hackage in the `haskus-system-build
<https://hackage.haskell.org/package/haskus-system-build>`_ package. You can
install it from here using your favorite method or install the latest version
from source as follows:

.. code:: bash

   > git clone https://github.com/haskus/haskus-system.git
   > cd haskus-system
   > stack install haskus-system-build

Using this method will install the program into ``~/.local/bin``. Be sure to add
this path to your ``$PATH`` environment variable.


------------------------------------------------------------------------------
Starting a new project
------------------------------------------------------------------------------

To start a new project, enter a new directory and use the ``init`` command:

.. code:: bash

   > mkdir my-project
   > cd my-project
   > haskus-system-build init

It downloads the default system template into the current directory. It is
composed of 4 files:

.. code:: bash

   > find . -type f
   ./stack.yaml
   ./src/Main.hs
   ./my-system.cabal
   ./system.yaml

``src/Main.hs``
~~~~~~~~~~~~~~~

This the system code. For now it's only a simple program that prints the "Hello
World" string in the kernel console, waits for a key press and then shutdowns the
system.

.. code:: haskell

   import Haskus.System
   
   main :: IO ()
   main = runSys <| do
   
      -- Initialize the terminal
      term <- defaultTerminal
   
      -- print a string on the standard output
      writeStrLn term "Hello World!"
   
      -- wait for a key to be pressed
      waitForKey term
   
      -- shutdown the computer
      powerOff_

``my-system.cabal``
~~~~~~~~~~~~~~~~~~~

This is the package configuration file that has been tweaked to build valid
executables (use static linking, etc.)

``stack.yaml``
~~~~~~~~~~~~~~

This is Stack configuration file with the required dependencies on Haskus
packages.


``system.yaml``
~~~~~~~~~~~~~~~

This is the ``haskus-system-build`` configuration file.

.. code:: yaml

   linux:
      source: tarball
      version: 4.11.3
      options:
         enable:
            - CONFIG_DRM_BOCHS
            - CONFIG_DRM_RADEON
            - CONFIG_DRM_NOUVEAU
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
system as being the ramdisk "init" program and some QEMU configuration. The
selected Linux kernel will be automatically downloaded and built with the given
options in the following steps.

------------------------------------------------------------------------------
Building and Testing
------------------------------------------------------------------------------

Now let's try the system within QEMU:

.. code:: bash

   > haskus-system-build test

On the first execution, this command downloads and builds everything required to
test the system so it can take quite some time. Then QEMU's window should pop up
with our system running in it.

On following executions building is much faster because the tool reuses
previously built artefacts (in particular the Linux kernel) if the configuration
hasn't changed.

If you only want to build without launching QEMU, use the build command:

.. code:: bash

   > haskus-system-build build

------------------------------------------------------------------------------
Distributing and testing on real computers
------------------------------------------------------------------------------

This tutorial wouldn't be complete without an exaplanation of how to distribute
your system to other people. We obviously don't want them to build it from
source.

Physical distribution
~~~~~~~~~~~~~~~~~~~~~

You can easily distribute your system on a bootable storage device (e.g. USB
stick). To do that, you only have to install your system on an **empty** storage
device.

**Warning: data on the device will be lost! Don't do that if you don't know what
you are doing!**

To install your system on the device whose device file is ``/dev/sde``:

.. code:: bash

   > haskus-system-build make-device --device /dev/sde

Note that you have to be in the *sudoers* list to access the device.

ISO image distribution
~~~~~~~~~~~~~~~~~~~~~~

Another distribution method is to create an ISO image that you can distribute
online or burn on CD/DVD.

.. code:: bash

   > haskus-system-build make-iso
   ...
   ISO image: .system-work/iso/my-system.iso

Note that you can test the ISO image with QEMU before you ship it:

.. code:: bash

   > haskus-system-build test-iso

This allows you to test the boot-loader configuration.
