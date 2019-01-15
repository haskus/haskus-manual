==============================================================================
Literals and embedding
==============================================================================

Sometimes we know at compile time what the (initial) contents of a :ref:`Buffer
<buffer>` is. It would be cumbersome to have to allocate the buffer and to write
its content word by word at program initialization or before using the buffer.
This chapter presents the alternatives.

------------------------------------------------------------------------------
List literals
------------------------------------------------------------------------------

If the buffer is very small, we can use the ``OverloadedLists`` extension to
create an immutable buffer from a list of bytes.  It allows the creation of
small unpinned immutable buffers into GHC's heap (aka ``BufferI``).

.. code:: haskell

   {-# LANGUAGE OverloadedLists #-}

   b :: BufferI
   b = [25,26,27,28] -- Word8 values

This should only be used for very small buffers. First, because it is not the
most efficient way to build a buffer: the actual buffer will be created when it
is first used. Second, because it is very cumbersome to list bytes' values in a
list.

------------------------------------------------------------------------------
Embedding files as buffers
------------------------------------------------------------------------------

You can embed an external file (or some part of it) into your executable. At
runtime you can access it as a normal external buffer (mutable or not).

.. code:: haskell

   {-# LANGUAGE TemplateHaskell #-}

   import Haskus.Memory.Buffer
   import Haskus.Memory.Embed

   let b = $(embedFile "myfile.bin"
               True       -- is the resulting buffer mutable or not
               (Just 8)   -- optional alignment constraint
               (Just 10)  -- optional offset in the file
               (Just 128) -- optional number of bytes to include
            )

------------------------------------------------------------------------------
Embedding buffers using Template Haskell
------------------------------------------------------------------------------

If you know how to build your buffer at compile time, you can build it and embed
it into the executable with Template Haskell by using ``embedBuffer``.

.. code:: haskell

   import Haskus.Memory.Embed

   embedBuffer
      :: Buffer mut pin fin heap -- ^ Source buffer
      -> Bool       -- ^ Should the embedded buffer be mutable or not
      -> Maybe Word -- ^ Optional alignement constraint
      -> Maybe Word -- ^ Optional offset in the source buffer
      -> Maybe Word -- ^ Optional number of bytes to include
      -> Q Exp      -- ^ BufferE or BufferME, depending on mutability parameter

