.. haskus-manual documentation master file, created by
   sphinx-quickstart on Fri Jun  2 16:16:29 2017.
   You can adapt this file completely to your liking, but it should at least
   contain the root `toctree` directive.

Haskus manuals
==============

haskus-system
-------------

``haskus-system`` is a framework written in Haskell that can be used for system
programming. Fundamentally it is an experiment into providing an integrated
interface leveraging Haskell features (type-safety, STM, etc.) for the whole
system: input, display, sound, network, etc.

Volume 1: basic architecture and building
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

.. toctree::
   :maxdepth: 2
   :numbered:

   system/volume1/intro
   system/volume1/approach
   system/volume1/build_tool
   system/volume1/booting
   system/volume1/reference/system_yaml
   system/volume1/reference/haskus_system_build

Volume 2: system programming guide
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

In this volume, we describe the current state of the high-level interfaces
provided by the ``haskus-system`` to control and use the computer.

.. toctree::
   :maxdepth: 2
   :numbered:

   system/volume2/modules_overview
   system/volume2/using/sys_monad
   system/volume2/using/devices
   system/volume2/graphics/overview
   system/volume2/x86

haskus-binary
-------------

.. toctree::
   :maxdepth: 2

   binary

haskus-utils
-------------

Variant/Flow

.. toctree::
   :maxdepth: 2
