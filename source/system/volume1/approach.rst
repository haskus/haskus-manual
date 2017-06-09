Discussing the approach
=======================

The ``haskus-system`` framework aims to help writing systems in Haskell.
Writing a new operating system from scratch is obviously a huge task that we
won't undertake. Instead, pragmatically, we build on the Linux kernel to develop
the ``haskus-system``.

   The fact that it is based on the Linux kernel shouldn't confuse you: we don't
   have to let applications directly access it through a UNIX-like interface! This
   is similar to the approach followed by Google with Android: the Linux kernel is
   used internally but applications have to be written in Java and they have to use
   the Android interfaces. The difference is that we are using Haskell.

The ``haskus-system`` framework and the systems using it are written with the
Haskell language. We use GHC to compile Haskell codes, hence we rely on GHC's
runtime system. This runtime system works on a bare-bones Linux kernel and
manages memory (garbage collection), user-space threading,  asynchronous I/O,
etc.


Portability
-----------

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

Performance
-----------

Using a high-level language such as Haskell is a trade-off between performance
and productivity. Just like using C language instead of plain assembly language
is. Moreover in both cases we expect the compilers to perform optimizations that
are not obvious or that would require complicated hard to maintain codes if they
were to be coded explicitly.

GHC is the Haskell compiler we use. It is a mature compiler still actively
developed. It performs a lot of optimizations. In particular, it performs
inter-modules optimizations so that well-organized modular code doesn't endure
performance costs.

Haskell codes are compiled into native code for the architecture (i.e., there is
no runtime interpretation of the code). In addition, it is possible to use LLVM
as a GHC backend to generate the native code.

The generated native codes are linked with a runtime system provided by GHC that
manages:

* Memory: garbage collection
* Threading: fast and cheap user-space threading
* Software transactional memory (STM): safe memory locking
* Asynchronous I/O: non-blocking I/O interacting with the threading system

Performance-wise, this is a crucial part of the stack we use. It has been
carefully optimized and it is tunable for specific needs. It is composed of
about 40k lines of C code.

As a last resort, it is still possible to call codes written in other languages
from Haskell through the Foreign Function Interface (FFI) or by adding a Primary
Operation (primop). ``haskus-system`` uses these mechanisms to interact with
the Linux kernel.

   It seems to us that this approach is a good trade-off. As comparison points,
   most UNIX-like systems rely on unsafe interpreted shell scripts (init systems,
   etc.); Google's Android (with Dalvik) used to perform runtime bytecode
   interpretation and then just-in-time compilation, currently (with ART) it still
   uses a garbage collector; Apple's platforms rely on a garbage collection variant
   called "automatic reference counting" in Objective-C and in Swift languages
   (while it might be more efficient, it requires much more care from the
   programmers); JavaScript based applications and applets (unsafe language, VM,
   etc.) tend to generalize even on desktop.


Productivity
------------

Writing system code in a high-level language such as Haskell should be much more
productive than writing it in a low-level language like C.

* Most of the boilerplate code (e.g., error management, logging) can be
  abstracted away.

* Thanks to the type system, many errors are caught during the compilation,
  which is especially useful with system programming because programs are harder
  to debug using standard methods and tools. Moreover it makes codes much easier
  to maintain because the compiler checks many more things during refactoring.

* High-level code is often dense and terse. Hence we can show full working code
  snippets in the documentation that you can quickly copy. Moreover, writing
  system code is much more fun and we can quickly get enjoyable results.

Durability and Evolution
------------------------

Our approach should be both durable and evolutive. Durable because we only use
mature technology: Linux and GHC developments both started in early 1990s and
are still very active. The only new layer in the stack is the ``haskus-system``
framework.  All of these are open-source free software, ensuring long-term
access to the sources.

The approach is evolutive: Haskell language is evolving in a controlled way with
GHC's extensions (and a potential future Haskell standard revision); GHC as a
compiler and a runtime system is constantly improving and support for new
architectures could be added; Linux support for new hardware and new
architectures is constantly enhanced and specific developments could be done to
add features useful for the ``haskus-system`` (or your own system on top of it).

The ``haskus-system`` framework itself is highly evolutive. First it is new and
not tied to any standard. Moreover code refactoring in Haskell is much easier
than in low-level languages such as C (thanks to the strong typing), hence we
can easily enhance the framework interfaces as user code can easily be adapted.

Single Code Base & Integration
------------------------------

In our opinion, a big advantage of our approach is to have an integrated
framework whose source is in a single code base. It makes it much easier to
evolve at a fast pace without having to maintain interface compatibility between
its internal components. Moreover, refactoring is usually safe and relatively
easy in Haskell, so we could later split it into several parts if needed.

   As a comparison point, usual Linux distributions use several system services and
   core libraries, most of them in their own repository and independently
   developed: ``libc``, ``dbus``, ``udev``, ``libdrm``, ``libinput``,
   ``Mesa/X11/Wayland``, ``PulseAudio``, etc. It is worth noting that the issue has
   been identified and that an effort has been recently made to reduce the
   fragmentation and to centralize some of them into a more integrated and coherent
   framework: ``systemd``.

Having a single codebase written with a high-level language makes it easier to
find documentation, to understand how things work (especially the interaction
between the different components) and to make contributions.

Standards
---------

The ``haskus-system`` can only be used on top of the Linux kernel. It doesn't
try to follow some standards (``UNIX``, ``POSIX``, ``System V``, etc.) to be
portable on other kernels. In our opinion, these standards have been roadblocks
to progress in system programming because system services and applications are
usually designed to follow the least common standards to ensure portability. For
instance, useful features specific to the Linux kernel may not be used because
some BSD kernels do not support them [See also the heated debates about
``systemd`` requiring Linux specific features]. With our approach, we can use
every feature of the Linux kernel and develop new ones if needed.

It is often stated that programs should conform to the "UNIX philosophy":
each program should do only one thing and programs must be easily composable.
Despite this philosophy, ``UNIX`` systems often stand on feet of clay: programs are
composed with unsafe shell scripts and data exchanged between programs are
usually in weakly structured plain text format.

In our opinion, functional programming with strong typing is much more principled
than the "UNIX philosophy": functions are by nature easily composable and their
interfaces are well-described with types. In addition, we are not limited to
plain text format and the compiler ensures that we are composing functions in
appropriate ways.

   As an example, compare this with ``UNIX`` standard commands such as ``ls`` which
   include many result sorting flags while the ``sort`` command could be used
   instead: the weakly structured output of the ``ls`` command makes it very
   inconvenient to indicate on which field to sort by (*hard to compose*).
   Moreover, the output of the ``ls`` command mustn't change ever, otherwise many
   tools relying on it may be broken (*not evolutive*). This is because most
   commands do two things: compute a result and format it to be outputted, while
   they should only do the first (according to the ``UNIX`` philosophy). We don't
   have this issue because we use type-checked data types instead of plain text.

Even if the ``haskus-system`` is in a single code base, its functions can be
used in other Haskell programs just by importing its modules. The compiler
statically checks that functions are appropriately called with valid parameters.

   Compare this with the usual interface between two ``UNIX`` programs: parameters
   from the first program have to be serialized and passed on the command-line
   (with all the imaginable limitations on their sizes); then the second program
   has to parse them as well as its standard input, to handle every error case (missing
   parameter, invalid parameter, etc.), and to write the result; finally the first
   program has to parse the outputs (both ``stdout`` and ``stderr``) of the second
   one and to react accordingly. For such a fundamental concept, there is a lot of
   boilerplate code involved and many potential errors lurking in it.


Building And Testing
--------------------

Our approach allows us to quickly have a working prototype that can be tested in
an emulated environment (e.g., with ``QEMU``).

   As a comparison point, building a minimal usual Linux distribution from scratch
   is very cumbersome as we can read in the "`Linux From Scratch
   <http://www.linuxfromscratch.org/lfs>`_" book. A lot of different packages have
   to be downloaded from various places, patched, configured, built and installed.
   Even if our approach is currently far from being on par with a usual Linux
   distribution, we expect it to stay much more simpler to build.

Proprietary Drivers
-------------------

Some vendors do not provide open-source drivers nor documentation for their
hardware. Instead they provide pre-compiled libraries and/or kernel modules.  As
they presuppose the use of some system libraries and services (``OpenGL``,
``X11``, etc.), ``haskus-system`` doesn't support them.
