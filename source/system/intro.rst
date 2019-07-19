==============================================================================
Introduction to ``haskus-system``
==============================================================================

**haskus-system** is a framework to develop systems based on the Linux kernel.
Its main selling points are:

* written in Haskell (systems are written in Haskell too)
* automatic building and testing (within QEMU) with the :ref:`haskus-system-build <haskus-system-build-tool>`
* easy to use and to modify: `single code base in a Git repository
  <https://github.com/haskus/haskus-system/>`_ , documentation
* reproducible builds: pinned versions (Linux kernel and Haskell dependencies)

``haskus-system`` is based *directly* and *exclusively* on the Linux kernel.
Hence, it doesn't rely on usual user-space kernel interfaces (e.g., libdrm,
libinput, X11, wayland, etc.) to communicate with the kernel.

.. toctree::
   :maxdepth: 1
   :numbered:

   intro/overview
   intro/portability
   intro/performance
   intro/assets
