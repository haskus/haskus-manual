==============================================================================
Generic buffers
==============================================================================

The top-most elements of the :ref:`graphics pipeline <graphics_pipeline>` are
Buffers. A Buffer is a memory region accessible by the graphics chipset. In this
case they are used to store pixel color components (e.g. 32-bit RGBA values).

There are two general kinds of buffers:

* driver specific buffers: they can be used to fully exploit the features of the
  device but they use different APIs depending on the device driver.

* generic buffers: these buffers use the same API for all devices supporting
  them but they can't be used to exploit the most advanced features of the
  device.

In this chapter we present the *generic buffers* as they are simple to use and
not tied to a specific device.
