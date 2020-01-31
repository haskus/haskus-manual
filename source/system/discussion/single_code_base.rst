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


