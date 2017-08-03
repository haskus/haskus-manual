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

.. toctree::
   :maxdepth: 2
   :numbered:
   :caption: System - Volume 1: building guide

   system/volume1/intro
   system/volume1/approach
   system/volume1/automatic_building
   system/volume1/manual_building
   system/volume1/reference/system_yaml
   system/volume1/reference/haskus_system_build

In volume 2, we describe the high-level interfaces provided by the
``haskus-system`` to control and use the computer.

.. toctree::
   :maxdepth: 2
   :numbered:
   :caption: System - Volume 2: programming guide

   system/volume2/using/sys_monad
   system/volume2/using/devices
   system/volume2/graphics/overview
   system/volume2/graphics/compositing

In volume 3, we describe the internals of ``haskus-system``.

.. toctree::
   :maxdepth: 2
   :numbered:
   :caption: System - Volume 3: internals

   system/volume3/modules_overview
   system/volume3/x86
   system/volume3/x86/encoding

haskus-binary
-------------

.. toctree::
   :maxdepth: 2
   :numbered:
   :caption: Binary

   binary

haskus-utils
-------------

Variant/Flow

.. toctree::
   :maxdepth: 2
   :numbered:
   :caption: Utils
