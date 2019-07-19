==============================================================================
Overview
==============================================================================

Typical computer programs don't directly deal with hardware components
(processor, video chipsets, keyboard, mouse, network chipset, storage, etc.).
Instead they rely on a lot of abstractions provided by other software. The main
one being the `kernel
<https://en.wikipedia.org/wiki/Kernel_(operating_system)>`_ which directly
handles the hardware with device specific drivers. It provides a common
framework that all the other programs have to use to access the hardware.

`Linux <https://en.wikipedia.org/wiki/Linux_kernel>`_ is one of such kernels and
it is the one we use. Typical computer programs usually don't use the interfaces
provided by the Linux kernel directly. They use abstractions built on top of
them and provided by system libraries which compose an `operating system (OS)
<https://en.wikipedia.org/wiki/Operating_system>`_.

Many operating systems based on the Linux kernel use more or less the same
abstractions on top of it: these are the Unix-like `Linux distributions
<https://en.wikipedia.org/wiki/Linux_distribution>`_. Examples of abstractions
commonly used by these distributions: libc, libinput, Wayland/X11 server,
PulseAudio, dbus, etc. As they differ only in minor ways, they can execute the
same applications (Firefox, LibreOffice, etc.).

It is also possible to build non Unix-like operating systems on top of Linux.
These operating systems may provide totally different abstractions to the
applications. One example of such OS is `Android
<https://en.wikipedia.org/wiki/Android_(operating_system)>`_ which mainly
supports applications written in Java that use specific interfaces to
communicate with the hardware.

``haskus-system`` provides interfaces to the Linux kernel (hence to the
hardware) in Haskell and builds on them to provide higher-level interfaces. You
can use these interfaces to build custom systems.  Then it is up to you to
decide if your system has the concept of "application" or not: you may design
domain specific systems which provide a single domain specific "application".
