=====================
Video displays basics
=====================

A video display usually shows a rectangle of pixels. When connected to a
computer, it is managed by a graphics chipset which indicates to the video
display the color of each pixel of the current image to show several times per
second.

The algorithm used by the graphics chipset is roughly:

.. code:: text

   For each pixel in the current image
      Read its color from memory
      Convert the color into something intelligible for the video display
      Send the converted pixel color to the video display

   Wait until it's time to send the next image (vertical blanking interval)

Refresh rates
-------------

The number of images (or frames) sent to the video display depends on its
refresh rate. Most video displays have a fixed (configurable) refresh rate. For
instance 60Hz (60 frames per second).

Newer video displays may also support variable refresh rates where the delay
between two frames may depend on the next frame being ready (or a timeout if it
isn't even after some time).

Switching between frames
------------------------

If we modify the memory containing the pixel colors while the graphics chipset
is reading it, the video display may show incoherent pixels (e.g. pixels from
two different frames). This is called *tearing*. We usually want to avoid it.

The idea is to modify the pixel colors only while the graphics chipset is
waiting between two transfers. For historical reasons this period of time is
called the `vertical blanking interval (VBI or VBLANK)
<https://en.wikipedia.org/wiki/Vertical_blanking_interval>`_.

If the software can't render a frame fast enough during the VBI, we usually want
the previous frame to be sent again. In order to do that, we use two buffers
(*double-buffering*): one buffer contains the current frame that is sent
repeatedly to the video display and the other contains the frame the software is
currently rendering. Once the next frame is ready we only have to switch the
buffer pointer (called *page-flipping*) during the VBI to inverse the roles of
the two buffers.

If the software renders frames too fast we can either block it until
page-flipping occurs or we can use *triple-buffering*: one buffer contains the
current frame as before, the second contains a pending frame (if any) which is a
frame ready to be displayed, and the third one contains the frame being rendered
as before. When the rendering in the third buffer is done, there is a switch
between the second and the third buffer (i.e. one of the rendered frame may not
be displayed at all if there was already a pending frame). During the VBI, if
there is a pending frame in the second buffer, there is a switch between the
first and the second buffers.

Plane composition
-----------------

Instead of using a single source for the pixel colors, some graphics chipsets
allow the use of several pixel sources that are blended/composed together.

A *plane* describes a portion of the video display surface that uses a specific
source of pixel colors. Basic graphics chipsets only have a single primary plane
that occupies the whole video display surface; other graphics chipsets may have
several other planes with different properties (pixel formats, dimensions,
rotation, scaling, etc.).

This is particularly useful for what is called *hardware cursor*. A small cursor
plane is dedicated to display the mouse cursor so when the mouse moves we only
have to change the position of the cursor plane independently of the other
planes. It makes the cursor much more responsive because this operation is very
cheap.

Planes can also be used to render hardware decoded videos, overlays, etc.

Summary
-------

The graphics chipset sends the pixel colors from the memory to the connected
video display several times per second (depending on the refresh rate).

The graphics chipset supports at least one primary plane but it can also support
additional planes (overlay, cursor, etc.) with additional properties (scaling,
rotation, different pixel format, etc.).

The software is responsible of producing pixel colors for each plane. To avoid
tearing, the switch from one frame to the other must be done during the vertical
blanking interval (VBI or VBLANK). Double- or triple-buffering can be used for
this purpose.
