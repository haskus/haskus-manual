==============================================================================
Linux display model
==============================================================================

------------------------------------------------------------------------------
Host buffers ("software" rendering on the CPU)
------------------------------------------------------------------------------

Most GPU drivers support pixel buffers stored in host memory (``HostBuffer``,
"dumb buffers" in Linux terminology).  These buffers are not accessible by the
GPU hence we can't use the GPU to perform computation into them. However, we can
easily use them to perform softawre/CPU rendering.

Applications only have to map the contents of the ``HostBuffer`` into their
memory address spaces and to modify it to change what is displayed.

------------------------------------------------------------------------------
Further Reading
------------------------------------------------------------------------------

The two main acronyms for Linux's display model are KMS (standing for "kernel
mode-setting") and DRM (standing for "direct rendering maanger").

As explained in the :ref:`device-management` section, device drivers can support
the ``ioctl`` system call to handle device specific commands from the
user-space. The display interface is almost entirely based on it. Additionally,
``mmap`` is used to map graphic card memory in user-space and ``read`` is used
to read events (V-Blank and page-flip asynchronous completion).

In usual Linux distributions, the ``libdrm`` library provides an interface over
these system calls. You can learn about the low-level interface by reading the
``drm`` manual (``man drm``, ``man drm-kms``, etc.) or its `source code
<https://cgit.freedesktop.org/mesa/drm/>`_.

David Herrmann has written `a good tutorial
<https://dvdhrm.wordpress.com/?s=drm-mode-setting>`_ explaining how to use the
legacy low-level display interface in the form of C source files with detailed
comments. While some details of the interface have changed since he wrote it
(e.g., the way to flip frame buffers and the atomic interface), it is still a
valuable source of information.

The newer atomic interface is described in an `article
<https://lwn.net/Articles/653071}>`_ `series
<https://lwn.net/Articles/653466/>`_ on LWN called "Atomic mode setting design
overview" (August 2015) by Daniel Vetter.

`Wayland <http://wayland.freedesktop.org>`_ is the new display system for usual
Linux based distributions. It can be a great source of inspiration and of
information.

You can also read the Linux kernel code located in ``drivers/gpu/drm`` in the
kernel sources.

Multi-GPU is supported by Linux. In particular:

* Buffer sharing is supported with `DRM Prime <https://01.org/linuxgraphics/gfx-docs/drm/drm-memory-management.html\#drm-prime-support>`_

* GPU switching is supported with `vga_switcheroo <https://01.org/linuxgraphics/gfx-docs/drm/vga_switcheroo.html>`_
