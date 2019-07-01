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
The whole source code can be found `here
<https://github.com/haskus/haskus-system/blob/master/haskus-system-examples/src/tutorial/TutCreateGenericFrame.hs>`_.

Generic buffers have one very useful property: they can be mapped into the
process memory so that modifying them just consists in writing into memory.

To allocate a generic buffer, we can use `createGenericBuffer` function. It
takes a width and an height in pixels and the number of bits per pixels (must be
a multiple of 8). It returns a ``GenericBuffer`` object which contains the
effective size of the buffer, the pitch (effective width in bytes of a line) and
a ``ForeignPtr`` to the buffer mapped in the process memory.

However we often want to create a Frame and the buffers according to the Frame
format. Hence in the code example we use ``createGenericFrame`` instead which
does all of this. Then it displays information about the allocated Frame and
Buffers.

Example of run into QEMU with Linux 5.1.15:

.. code:: text

   > git clone https://github.com/haskus/haskus-system.git
   > cd haskus-system/haskus-system-examples
   > haskus-system-build test --init TutCreateGenericFrame

   Frame 35
     Width:  1024 pixels
     Height: 768 pixels
     Pixel format: XRGB8888 (LittleEndian)
     Flags: []
     FrameBuffer 0:
       - Buffer handle: 1
       - Pitch: 4096
       - Offset: 0
       - Modifiers: 0
       - Buffer specific details:
          - Type: generic buffer
          - Width:  1024 pixels
          - Height: 768 pixels
          - Bits per pixels: 32
          - Flags: 0
          - Handle: 1
          - Pitch: 4096 bytes
          - Size: 3145728 bytes
          - Address: 0x00007efd406f5000

   [    0.984017] reboot: Power down
