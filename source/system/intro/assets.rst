==============================================================================
Main assets
==============================================================================

Productivity
------------

Writing system code in a high-level language such as Haskell should be much more
productive than writing it in a low-level language like C.

* High-level code is often more concise and most of the boilerplate code (e.g.,
  error management, logging, memory management) can be abstracted away. Fully
  working code examples stay shorts and understandable. Writing system code is
  much more fun as we can quickly get enjoyable results (less irrelevant details
  to manage).

* Many errors are caught during the compilation (type checking) which is
  especially useful with system programming because programs are harder to debug
  using standard methods (printf) and tools (gdb).

* Code is easier to refactor thanks to type-checking, hence more maintenable.


Durability and Evolution
------------------------

Our approach should be both durable and evolutive. Durable because we only use
mature technology: Linux and GHC developments both started in early 1990s and
are still very active. The only new layer in the stack is ``haskus-system``
framework.  All of these are open-source free software, ensuring long-term
access to the sources.

The approach is evolutive: Haskell language is evolving in a controlled way with
GHC's extensions (and a potential future Haskell standard revision); GHC as a
compiler and a runtime system is constantly improving and support for new
architectures could be added; Linux support for new hardware and new
architectures is constantly enhanced and specific developments could be done to
add features useful for ``haskus-system`` (or your own system on top of it).

``haskus-system`` framework itself is highly evolutive. First it is new and
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

.. note::

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

``haskus-system`` can only be used on top of the Linux kernel. It doesn't
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

.. note::

   As an example, compare this with ``UNIX`` standard commands such as ``ls`` which
   include many result sorting flags while the ``sort`` command could be used
   instead: the weakly structured output of the ``ls`` command makes it very
   inconvenient to indicate on which field to sort by (*hard to compose*).
   Moreover, the output of the ``ls`` command must never change, otherwise many
   tools relying on it may be broken (*not evolutive*). This is because most
   commands do two things: compute a result and format it to be displayed, while
   they should only do the first (according to the ``UNIX`` philosophy). We don't
   have this issue because we use type-checked data types instead of plain text.

Even if ``haskus-system`` is in a single code base, its functions can be
used in other Haskell programs just by importing its modules. The compiler
statically checks that functions are appropriately called with valid parameters.

.. note::

   Compare this with the usual interface between two ``UNIX`` programs:
   parameters from the first program have to be serialized as text and passed on
   the command-line (with all the imaginable limitations on their sizes); then
   the second program has to parse them as well as its standard input, to handle
   every error case (missing parameter, invalid parameter, etc.), and to write
   the result; finally the first program has to parse the outputs (both
   ``stdout`` and ``stderr``) of the second one and to react accordingly. For
   such a fundamental concept, there is a lot of boilerplate code involved and
   many potential errors lurking in it.


Building And Testing
--------------------

Our approach allows us to quickly have a working prototype that can be tested in
an emulated environment (e.g., with ``QEMU``). Especially with the
:ref:`haskus-system-build <haskus-system-build-tool>` that automatizes all of
the building steps.

.. note::

   As a comparison point, building a minimal usual Linux distribution from scratch
   is very cumbersome as we can read in the "`Linux From Scratch
   <http://www.linuxfromscratch.org/lfs>`_" book. A lot of different packages have
   to be downloaded from various places, patched, configured, built and installed.
   Even if our approach is currently far from being on par with a usual Linux
   distribution, we expect it to stay much more simpler to build.
