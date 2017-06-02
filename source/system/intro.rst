Introduction
============

A typical operating system can be roughly split into three layers:

* Kernel: device drivers, virtual memory management, process scheduling,
  etc.

* System: system services and daemons, low-level kernel interfaces, etc.

* Application: end-user applications (web browser, video player, games, etc.)

Kernel
------

``haskus-system`` is based directly and exclusively on the Linux kernel. Hence,

* it doesn't contain low-level kernel code (device driver, etc.)

* it doesn't rely on usual user-space kernel interfaces (e.g., libdrm, libinput,
  X11, wayland, etc.) to communicate with the kernel

Note however that programs using the ``haskus-system`` are compiled with GHC:
hence they still depend on GHC's runtime system (RTS) dependencies (libc, etc.).
Programs are statically compiled to embed those dependencies.

System
------

The first aim of the ``haskus-system`` framework is to make it easy to experiment
different approaches at the "system" level (which obviously has an impact on the
"application" level). In particular, we would like to be able to easily revisit
long-standing concepts, such as:

* Services and applications: integration with networks (Internet
  services, private cloud...), with hot-pluggable devices, etc. 

* System infrastructure: application management (distribution,
  installation), code sharing (libraries and linking), file systems,
  security, etc.

* User interface: interaction with the user (input and output).

The second aim is to make system programming more enjoyable and productive by
using a high-level language (Haskell) and by providing a hopefully coherent and
well-documented framework with interfaces that are easy to use.

Application
-----------

It is up to you to decide if your system has the concept of "application"!
