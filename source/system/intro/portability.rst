==============================================================================
Portability
==============================================================================

Writing a new kernel from scratch is obviously a huge task.  Pragmatically
``haskus-system`` relies on an existing kernel: Linux.

Note: the fact that ``haskus-system`` is based on the Linux kernel doesn't imply that
systems built with it have to provide a Unix-like interface to their
applications. This is similar to the approach followed by Google with Android:
the Linux kernel is used internally but applications have to be written in Java
and they have to use the Android interfaces.

``haskus-system`` framework and the systems using it are written with the
Haskell language. We use GHC to compile Haskell codes, hence we rely on GHC's
runtime system. This runtime system works on a bare-bones Linux kernel and
manages memory (garbage collection), user-space threading,  asynchronous I/O,
etc. The runtime system has non-Haskell dependencies (libc, etc.) which are
statically linked with systems.

**Supported hardware**

The portability is ensured by the Linux kernel. In theory we could use our
approach on any architecture supported by the Linux kernel. In practice, we also
need to ensure that GHC supports the target architecture.

In addition, ``haskus-system`` requires a thin architecture-specific layer
because Linux interface is architecture specific. Differences between
architectures include: system call numbers, some structure fields (sizes and
orders), the instruction to call into a system call and the way to pass system
call parameters (calling convention).

The following architectures are currently supported by each level of the stack:

* haskus-system: x86-64
* GHC: x86, x86-64, PowerPC, and ARM
* Linux kernel: many architectures

**Proprietary Drivers**

Some vendors do not provide open-source drivers nor documentation for their
hardware. Instead they provide pre-compiled libraries and/or kernel modules.  As
they presuppose the use of some system libraries and services (``OpenGL``,
``X11``, etc.), ``haskus-system`` doesn't support them.
