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



