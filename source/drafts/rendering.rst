==============================================================================
Graphics Rendering
==============================================================================


------------------------------------------------------------------------------
FrameBuffer Displaying
------------------------------------------------------------------------------

setControllerFB

------------------------------------------------------------------------------
Buffer Mapping
------------------------------------------------------------------------------


* Mapping buffers in user-space
* Raw drawing
* Example rectangle
* Dirty fb

------------------------------------------------------------------------------
Configuring The Display
------------------------------------------------------------------------------

* Mode settings
* automatic configuration for the connected devices in preferred modes, no cloning

Setting the display mode
~~~~~~~~~~~~~~~~~~~~~~~~

The following code shows a code that tries every available mode of
a connector in sequence: each mode stays enabled for 4 seconds. To be allowed to
change the display mode, we need to create an adapted framebuffer (i.e., a
source of pixel data). You can safely ignore that for now, we will explain it in
details in Section~\ref{sec:framebuffer}.

.. code::

   -- codes/src/DisplayModes.hs

* mode not supported by display device: garbage, black screen, error screen


------------------------------------------------------------------------------
Drawing On The Screen
------------------------------------------------------------------------------

* Generic framebuffer
* full drawing example

* Example DPMS (power-off)


------------------------------------------------------------------------------
Multiple-Buffering And V-Blank Synchronization
------------------------------------------------------------------------------

Computer screens usually have a fixed refresh rate. For instance in
Listing~\ref{lst:display_connectors_result} the first mode of the connected
display device has a (vertical) refresh rate of 60Hz. That is, 60 times per
second, the display device:

* copies the contents of the framebuffer on the screen line-by-line: **scan-out** period
* waits for some time until the next scan-out: **v-blank** period

Single Frame Buffer
~~~~~~~~~~~~~~~~~~~

Suppose we use a single frame buffer that is displayed on the screen. If we
modify its content during the scan-out period, some artifacts may appear on the
screen. For instance if we repeatedly clear the screen, then draw a filled
rectangle and finally draw a circle on top of it, the display may either show
the cleared screen, the rectangle alone or the rectangle with the circle. The
rectangle and the circle seem to flash: this is called a **flicker** effect.

* time to render a frame vs refresh period vs v-blank period


* Explanation scan-out (flikering?)
* Explanation multi-buffering
* Code framebuffer swap ("page flip")
* Explanation v-blank (tearing?)
* Code synchro v-blank (event v-blank)
* Note "async page flip" flag and "page flip complete" event
* Adaptive v-sync
* Dithering (frame rate control, TN panels 6-bits per RGB)

------------------------------------------------------------------------------
Advanced Topics
------------------------------------------------------------------------------

Atomic Configuration
~~~~~~~~~~~~~~~~~~~~

Some drivers support an atomic configuration operation. Instead of setting a
property at a time, several properties are set at once in a transaction: if a
proprety fails to be set, the whole transaction is reverted. It ensures that the
display configuration is never left in a transition state between two
configurations.\footnote{At the time of writing, ``haskus-system`` doesn't
support this feature.}

Gamma Table
~~~~~~~~~~~

* theory, tests avec qemu non concluants

Gamma correction consists in altering the way colors are displayed on a monitor.
For each possible value of each color channel (red, green, blue),we can define a
gamma factor. Usually there are 256 possible values per channel, but the
additional \texttt{controllerGammaTableSize} field gives the actual number of
values. Each factor is an unsigned 16-bit word.

* Word16 or 8.8 fixed float?

The following code shows how to retrieve and show the gamma look-up
table of a controller with ``getControllerGamma``. Similarly you can set
another gamma table with ``setControllerGamma``.

.. code::

   -- codes/src/DisplayGamma

Sub-Pixel Rendering
~~~~~~~~~~~~~~~~~~~

* Controller sub-pixel
* Used for fonts
* Vertical vs horizontal
* rotation property!
