==============================================================================
Overview
==============================================================================


A typical operating system can be roughly split into three layers:

* Kernel: device drivers, virtual memory management, process scheduling,
  etc.

* System: system services and daemons, low-level kernel interfaces, etc.

* Application: end-user applications (web browser, video player, games, etc.)

When you use ``haskus-system``, it is as follow:

* Kernel: Linux

* System: your own system built using ``haskus-system``

* Application: you decide if your system has the concept of "application" or not



``haskus-system`` is based *directly* and *exclusively* on the Linux kernel.
Hence, it doesn't rely on usual user-space kernel interfaces (e.g., libdrm,
libinput, X11, wayland, etc.) to communicate with the kernel.

.. note::

   Systems built with ``haskus-system`` are compiled with GHC, hence they still
   have non-Haskell dependencies: GHC's runtime system (RTS), libc, etc.
   Systems are statically compiled to embed those dependencies.

``haskus-system`` acts at the *system* level: it provides interfaces to the
Linux kernel (hence to the hardware) in Haskell and builds on them to provide
higher-level interfaces.  You can use these interfaces to build custom systems.
Then it is up to you to decide if your system has the concept of "application"
or not: you may design domain specific systems which provide a single
domain specific "application".
