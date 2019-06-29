================
How screens work
================

A screen connected to a computer is managed by a graphics chipset. The chipset
indicates to the screen the color of each pixel several times per second:

.. code:: haskell

   For each pixel
      Read its color from memory
      Transform it into something intelligible for the screen
      Send it to the screen

   Wait some time until it's time to send the next frame

Refresh rates
-------------

The number of frames sent to the screen depends on its refresh rate. Most
screens have a fixed (configurable) refresh rate. For instance 60Hz (60 frames
per second).

Newer screens may also support variable refresh rates where the delay between
two frames may depend on the next frame being ready (or a timeout if it isn't
even after some time).

Switching between frames (page-flipping)
----------------------------------------

If we modify the memory containing the pixel colors while the graphics chipset
is reading it, the screen may display incoherent pixels (e.g. pixels from two
different frames). This is called *tearing*. We usually want to avoid it.

The idea is to modify the pixel colors only while the graphics chipset is
waiting between two transfers. For historical reasons this period of time is
called the `vertical blanking interval (VBI or VBLANK)
<https://en.wikipedia.org/wiki/Vertical_blanking_interval>`_.

If the software can't render a frame fast enough during the VBI, we want the
previous frame to be sent again. In order to do that, we use two buffers
(*double-buffering*): one buffer contains the current frame and the other
contains the frame the software is currently rendering. Once the next frame
is ready we only have to switch the buffer pointer (called *page-flipping*)
during the VBI.

If the software renders frames too fast we can either block it until
page-flipping occurs or we can use *triple-buffering*: one buffer contains the
currently displayed frame, the second contains the pending frame (if any) and
the third one contains the currently being rendered one (if any). We the
rendering in the third buffer is done, there is a switch between the second and
the third buffer (i.e. one of the rendered frame won't be displayed at all).

Plane composition
-----------------

Instead of using a single source for the pixel colors, some graphics chipset
allow the use of several pixel sources that are blended/composed together.

A *plane* describes a portion of the screen that uses a specific source of
pixel colors. Basic graphics chipsets only have a single primary plane that
occupies the whole screen display range; other graphics chipsets may have
several other planes with different properties (pixel formats, dimensions,
rotation, scaling, etc.).

This is particularly useful for what is called *hardware cursor*. A small cursor
plane is dedicated to display the mouse cursor so when the mouse moves you only
have to change the position of the cursor plane independently of the other
planes. It makes the cursor much more responsive.

Planes can also be used to render hardware decoded videos, overlays, etc.

Summary
-------

The graphics chipset sends the pixel colors from the memory to the connected
screen several times per second (depending on the refresh rate).

The graphics chipset supports at least one primary plane but it can also support
additional planes (overlay, cursor, etc.) with additional properties (scaling,
rotation, different pixel format, etc.).

The software is responsible of producing pixel colors for each plane. To avoid
tearing, the switch from one frame to the other must be done during the vertical
blanking interval (VBI or VBLANK). Double- or triple-buffering can be used for
this purpose.
