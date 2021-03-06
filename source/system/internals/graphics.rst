==============================================================================
Graphics
==============================================================================

------------------------------------------------------------------------------
"Software" rendering (i.e. on the CPU)
------------------------------------------------------------------------------

For now in this manual we present a simple approach to display rendering:
basically the picture to display is generated by the CPU in host memory and then
transferred to the GPU memory (implicitly by using memory mapping). Most recent
graphic cards propose more efficient approaches: the picture to display is
generated and transformed directly by the graphic card. Instead of sending a
picture, the host sends commands or programs to be executed by the GPU.


Currently Linux doesn't propose a unified interface to advanced graphic card
capabilities from different vendors (these are usually handled by MESA in
user-space and accessed through the OpenGL interface). ``haskus-system`` doesn't
provide support for them yet.

------------------------------------------------------------------------------
Proprietary drivers
------------------------------------------------------------------------------

``haskus-system`` uses the Kernel Mode Setting (KMS) and the Direct
Rendering Manager (DRM) interfaces. In usual Linux distributions, some graphic
card manufacturers provide closed-source proprietary drivers that do not support
theses interfaces: they use a kernel module and user-space libraries that
communicate together by using a private protocol. The user-space libraries
provide implementations of standard high-level interfaces such as OpenGL and can
be used by rendering managers such as X.org. ``haskus-system`` doesn't offer a
way to use these drivers.

------------------------------------------------------------------------------
Further reading
------------------------------------------------------------------------------

The two main acronyms for Linux's display model are KMS (standing for "kernel
mode-setting") and DRM (standing for "direct rendering manager").

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
<https://lwn.net/Articles/653071/>`_ `series
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

https://www.elinux.org/images/4/45/Atomic_kms_driver_pinchart.pdf

KMS/DRM history:

* https://libv.livejournal.com/13443.html
* https://ppaalanen.blogspot.com/2014/06/from-pre-history-to-beyond-global.html
* https://lwn.net/Articles/653071 and https://lwn.net/Articles/653466/
