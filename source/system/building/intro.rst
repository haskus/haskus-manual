Introduction
============

``haskus-system`` is a framework written in Haskell that can be used for system
programming. Fundamentally it is an experiment into providing an integrated
interface leveraging Haskell features (type-safety, STM, etc.) for the whole
system: input, display, sound, network, etc.

The big picture
---------------

A typical operating system can be roughly split into three layers:

* Kernel: device drivers, virtual memory management, process scheduling,
  etc.

* System: system services and daemons, low-level kernel interfaces, etc.

* Application: end-user applications (web browser, video player, games, etc.)

**Linux kernel**

``haskus-system`` is based *directly* and *exclusively* on the Linux kernel. Hence,

* it doesn't rely on usual user-space kernel interfaces (e.g., libdrm, libinput,
  X11, wayland, etc.) to communicate with the kernel
* it doesn't contain low-level kernel code (device driver, etc.)


Note, however, that programs using the ``haskus-system`` are compiled with GHC:
hence they still depend on GHC's runtime system (RTS) dependencies (libc, etc.).
Programs are statically compiled to embed those dependencies.

**haskus-system**

``haskus-system`` acts at the *system* level: it provides interfaces to the
Linux kernel (hence to the hardware) in Haskell and builds on them to provide
higher-level interfaces (described in the Volume 2 of this documentation).

You can use these interfaces to build custom systems. Then it is up to you to
decide if your system has the concept of "application" or not: you may design
domain specific systems which provide a single "application".
