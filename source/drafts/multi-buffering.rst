==============================================================================
Multi-Buffering and vertical synchronization
==============================================================================

------------------------------------------------------------------------------
Vertical synchronization
------------------------------------------------------------------------------

Computer screens usually have a fixed refresh rate. For instance if the 
display device has a (vertical) refresh rate of 60Hz, it means that 60 times per
second, the display device:

* copies the contents of the framebuffer on the screen: scan-out period
* waits for some time until the next scan-out: `vertical blanking interval`_
  (v-blank interval)

If we modify the contents of the framebuffer that is displayed on the screen
during the scan-out period, some artifacts may appear on the screen. To avoid
this, we use `vertical synchronization`_ (v-sync or vsync): we only modify what
is displayed during the vertical blanking interval.

------------------------------------------------------------------------------
Multi-buffering
------------------------------------------------------------------------------

If we use a single framebuffer, modifying its contents only during the vertical
blanking interval may be difficult in practice as it imposes hard deadlines to
complete the modifications.

Instead, we can use several framebuffers:

* the *front buffer* that is currently used for display and that is not modified
* the *back buffer(s)* that can be modified

When a framebuffer is ready to be displayed, it becomes the "front buffer" and
the current front buffer becomes a "back buffer". The action of switching
framebuffers is called `page flipping`_.

The page flipping must occur during the v-blank interval, otherwise `screen
tearing`_ may appear: some part of the displayed image is taken from the first
buffer and some other part from the second one.

If we want to start rendering a new frame without waiting for the page flipping
to complete, we can use more than one back buffer (e.g., `triple buffering`_).

------------------------------------------------------------------------------
Synchronization issues
------------------------------------------------------------------------------

Rendering frames at the appropriate rate for the display may be difficult in
some cases:

* the frame is complex and the time to render it is longer than a vsync period
* the frame rate is imposed by the source (e.g., a video encoded at 24 FPS) and
  is not compatible with the display's refresh rate

Adaptive vertical synchronization
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

If the computer can't generate frames fast enough for some refresh rates (e.g.,
in a game), it may be better to temporarily disable vertical synchronization to
reduce the latency (hence avoiding stuttering) at the cost of some visual
artefacts. This is a trade-off between image quality and sufficiently fast frame
generation speed as you can't have both.

This is what NVIDIA names `adaptive vsync <http://www.geforce.com/hardware/technology/adaptive-vsync/technology>`_.

Incompatible rates
~~~~~~~~~~~~~~~~~~

There are several techniques to try to deal with incompatible frame rates, such
as:

* if a frame is displayed during several periods, take this duration into
  account when rendering animated objects
* add motion blur to animated objects
* use interlaced frames

Further reading :

* https://blog.fishsoup.net/2011/06/22/what-to-do-if-you-cant-do-60fps/
* https://blog.fishsoup.net/2012/11/28/avoiding-jitter-in-composited-frame-display/
* `Telecine <https://en.wikipedia.org/wiki/Telecine#Frame_rate_differences>`_
* `Motion interpolation <https://en.wikipedia.org/wiki/Motion_interpolation>`_


------------------------------------------------------------------------------
Programming
------------------------------------------------------------------------------

Events
~~~~~~

The kernel can send events to user-space to signal the beginning of v-blank
periods and to signal the completion of page flips.



.. TODO
   A solution
   * switch the source atomically
   * v-blank events
   Examples
   * simple rendering engine
   * link to the Clock example in appendices

.. TODO
   * time to render a frame vs refresh period vs v-blank period
   * Explanation scan-out (flikering?)
   * Explanation multi-buffering
   * Code framebuffer swap ("page flip")
   * Explanation v-blank (tearing?)
   * Code synchro v-blank (event v-blank)
   * Note "async page flip" flag and "page flip complete" event
   * Adaptive v-sync
   * Dithering (frame rate control, TN panels 6-bits per RGB)
   * mode not supported by display device: garbage, black screen, error screen

------------------------------------------------------------------------------
Further reading
------------------------------------------------------------------------------

* `Multiple buffering (Wikipedia)
  <https://en.wikipedia.org/wiki/Multiple_buffering#Page_flipping>`_
* `Vertical blanking interval (Wikipedia)
  <https://en.wikipedia.org/wiki/Vertical_blanking_interval>`_
* `Screen tearing (Wikipedia)
  <https://en.wikipedia.org/wiki/Screen_tearing>`_

.. _`page flipping`: https://en.wikipedia.org/wiki/Multiple_buffering#Page_flipping
.. _`screen tearing`: https://en.wikipedia.org/wiki/Screen_tearing
.. _`vertical blanking interval`: https://en.wikipedia.org/wiki/Vertical_blanking_interval
.. _`triple buffering`: https://en.wikipedia.org/wiki/Multiple_buffering#Triple_buffering
.. _`vertical synchronization`: https://en.wikipedia.org/wiki/Analog_television#Vertical_synchronization
