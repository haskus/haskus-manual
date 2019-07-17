.. _graphics_generic_buffers:

==============================================================================
Generic buffers and frames
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
avalable for many graphics chipsets. The source code for this chapter can be
found `here
<https://github.com/haskus/haskus-system/blob/master/haskus-system-examples/src/tutorial/TutGenericFrame.hs>`_.

Allocating Generic Buffers
--------------------------

To allocate a generic buffer, we can use `createGenericBuffer` function. It
takes a width and an height in pixels and the number of bits per pixels (must be
a multiple of 8). 

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

You can see that a ``GenericBuffer`` object contains the effective size of the
buffer (in bytes), the pitch (effective width of a line in bytes), an address
(explained in the next section), etc.

Hint: if we want to create a generic Frame for a full screen mode, we can pass a
``Mode`` to ``createGenericFullScreenFrame``. Frame and Buffers dimensions (in
pixels) are then obtained from the Mode.

Writing into Generic Buffers
----------------------------

Generic buffers have a very useful property: they can be mapped into the process
memory. ``haskus-system`` automatically maps them when they are created so you
don't have to worry about doing it.

The mapped region address is stored in a ``ForeignPtr`` which you can use with:
``withGenericBufferPtr`` (or ``withGenericFrameBufferPtr`` wrapper). For
example:

.. code:: haskell

   -- fill the frame with a color
   withGenericFrameBufferPtr fb \ptr ->
      forEachFramePixel frame \x y -> do
         let off = frameBufferPixelOffset fb 4 x y -- 4 is pixel component size in bytes
         pokeByteOff (castPtr ptr) (fromIntegral off) (color :: Word32)

However with generic buffers it is even easier: there is the
``forEachGenericFramePixel`` wrapper that does these tricky pointer computations
for us. The code example uses it as follows:

.. code:: haskell

      -- fill the generic frame with a color
      -- (0 is the FrameBuffer index)
      forEachGenericFramePixel frame 0 \_x _y ptr ->
         poke ptr (0x316594 :: Word32) -- write a XRGB8888 color
