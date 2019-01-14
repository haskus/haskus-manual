==============================================================================
Buffer
==============================================================================

A ``Buffer`` object represents some allocated memory, i.e. a set of consecutive
memory cells. It has the following type:

.. code:: haskell

   data Buffer (mut :: Mutability) (pin :: Pinning) (fin :: Finalization) (heap :: Heap)

It's a GADT whose parameters have the following meaning:

.. code:: haskell

   -- | Is the buffer mutable or not?
   data Mutability
      = Mutable   -- ^ Memory cells are mutable
      | Immutable -- ^ Memory cells are immutable

   -- | Is the buffer pinned into memory?
   data Pinning
      = Pinned    -- ^ The buffer has a fixed associated memory address
      | NotPinned -- ^ The buffer contents can be freely moved to another address

   -- | Is the buffer automatically garbage collected?
   data Finalization
      = Collected    -- ^ Automatically collected by the garbage-collector
      | Finalized    -- ^ Finalizers are run just before GCing
      | NotFinalized -- ^ Not managed at all

   -- | Allocation heap
   data Heap
      = Internal -- ^ GHC heap
      | External -- ^ External heap

If you have read the sections about :ref:`memory allocation in general
<memory_allocation>` and  :ref:`memory allocation in GHC
<memory_allocation_ghc>`, most parameters must be self explanatory, except for
``Finalized`` :ref:`explained below <buffer_finalizers>`.

We define the following type aliases for the different buffer variants:

.. code:: haskell

   type BufferI   = Buffer 'Immutable 'NotPinned 'Collected    'Internal
   type BufferP   = Buffer 'Immutable 'Pinned    'Collected    'Internal
   type BufferM   = Buffer 'Mutable   'NotPinned 'Collected    'Internal
   type BufferMP  = Buffer 'Mutable   'Pinned    'Collected    'Internal
   type BufferME  = Buffer 'Mutable   'Pinned    'NotFinalized 'External
   type BufferE   = Buffer 'Immutable 'Pinned    'NotFinalized 'External
   type BufferF   = Buffer 'Immutable 'NotPinned 'Finalized    'Internal
   type BufferPF  = Buffer 'Immutable 'Pinned    'Finalized    'Internal
   type BufferMF  = Buffer 'Mutable   'NotPinned 'Finalized    'Internal
   type BufferMPF = Buffer 'Mutable   'Pinned    'Finalized    'Internal
   type BufferMEF = Buffer 'Mutable   'Pinned    'Finalized    'External
   type BufferEF  = Buffer 'Immutable 'Pinned    'Finalized    'External

------------------------------------------------------------------------------
Buffer size
------------------------------------------------------------------------------

Buffer size can be queried with:

.. code:: haskell

   bufferSizeIO :: MonadIO m => Buffer mut pin fin heap -> m Word
   bufferSize   :: BufferSize a => a -> Word

``bufferSize`` is pure and as such it can't be used with internal mutable
buffers which can be resized. All the other buffers provide a ``BufferSize``
instance.


.. _buffer_finalizers:

------------------------------------------------------------------------------
Finalizers
------------------------------------------------------------------------------

A finalizer is just a side-effecting function that can be associated to a data.
When a data is to be collected by GHC's garbage collector, GHC executes its
associated finalizers if the data has some before releasing the data.  This
mechanism is especially useful in the domain of explicit memory management (e.g.
to release externally allocated memory), hence ``Buffers`` can be ``Finalized``
to ease the use of this mechanism. In particular it ensures execution order of
the finalizers.

We can make any buffer ``Finalized`` with the following function (idempotent for
already ``Finalized`` buffers):

.. code:: haskell

   makeFinalizable :: MonadIO m => Buffer mut pin f heap -> m (Buffer mut pin 'Finalized heap)

Then you can attach a finalizer with:

.. code:: haskell

   addFinalizer :: MonadIO m => Buffer mut pin 'Finalized heap -> IO () -> m ()

The latest added finalizers are executed first. Finalizers are not guaranteed to
run (e.g. if the program exits before the buffer is collected).

------------------------------------------------------------------------------
Allocation
------------------------------------------------------------------------------

Allocation in GHC heap
~~~~~~~~~~~~~~~~~~~~~~

Buffers allocated in GHC heap can be pinned or not. They are automatically
collected.

.. code:: haskell

   newBuffer              :: MonadIO m => Word -> m BufferM
   newPinnedBuffer        :: MonadIO m => Word -> m BufferMP
   newAlignedPinnedBuffer :: MonadIO m => Word -> Word -> m BufferMP

Allocation using system malloc
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

Buffers allocated by system "malloc" allocator are pinned and must be either
explicitly freed with ``Malloc.freeBuffer`` or can be automatically freed by a
finalizer (e.g. if they are allocated with ``Malloc.newFinalizedBuffer``).

.. code:: haskell

   import qualified Haskus.Memory.Allocator.Malloc as Malloc

   Malloc.newBuffer          :: MonadIO m => Word -> m (Maybe BufferME)
   Malloc.newFinalizedBuffer :: MonadIO m => Word -> m (Maybe BufferMEF)
   Malloc.freeBuffer         :: MonadIO m => BufferME -> m ()

Buffer freezing/thawing
~~~~~~~~~~~~~~~~~~~~~~~

Some buffers can be converted from mutable to immutable and vice versa. This is
unsafe as the original buffer mustn't be used anymore after this.

.. code:: haskell

   -- | Mutable to immutable
   unsafeBufferFreeze :: (Freezable a b, MonadIO m) => a -> m b

   -- | Immutable to mutable
   unsafeBufferThaw   :: (Thawable a b , MonadIO m) => a -> m b

------------------------------------------------------------------------------
Read/write
------------------------------------------------------------------------------

Several primitives are provided to read and to write buffer contents. Some
primitives have constraints on the buffer type to restrict their use. For
example, the type system ensures that we don't use writing primitives with
immutable buffers.

Reading/writing Word8
~~~~~~~~~~~~~~~~~~~~~

We can read a ``Word8`` value by providing an index/offset into the buffer:

.. code:: haskell

   bufferReadWord8IO :: MonadIO m => Buffer mut pin fin heap -> Word -> m Word8

This is done in the IO monad because the function is generic and supports both
mutable and immutable buffers. If we deal with immutable buffers, we can use the
following pure function instead:

.. code:: haskell

   bufferReadWord8 :: Buffer 'Immutable pin fin heap -> Word -> Word8

We can also write ``Word8`` into mutable buffers with:

.. code:: haskell

   bufferWriteWord8IO :: MonadIO m => Buffer 'Mutable pin fin heap -> Word -> Word8 -> m ()

Reading/writing Word16/Word32/Word64
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

Reading and writing ``Word16``, ``Word32`` or ``Word64`` could be expressed with
the primitives to read/write ``Word8``. However, most architectures provide
instructions to directly read/write larger words. Using them is much more
efficient than falling back to ``Word8`` primitives.

.. code:: haskell

   bufferReadWord16 :: Buffer 'Immutable pin fin heap -> Word -> Word16
   bufferReadWord32 :: Buffer 'Immutable pin fin heap -> Word -> Word32
   bufferReadWord64 :: Buffer 'Immutable pin fin heap -> Word -> Word64

   bufferReadWord16IO :: MonadIO m => Buffer mut pin fin heap -> Word -> m Word16
   bufferReadWord32IO :: MonadIO m => Buffer mut pin fin heap -> Word -> m Word32
   bufferReadWord64IO :: MonadIO m => Buffer mut pin fin heap -> Word -> m Word64

   bufferWriteWord16IO :: MonadIO m => Buffer 'Mutable pin fin heap -> Word -> Word16 -> m ()
   bufferWriteWord32IO :: MonadIO m => Buffer 'Mutable pin fin heap -> Word -> Word32 -> m ()
   bufferWriteWord64IO :: MonadIO m => Buffer 'Mutable pin fin heap -> Word -> Word64 -> m ()

Different architectures store the ``Word8`` composing larger words in different
orders (called ``Endianness``). When we use buffers to exchange data with other
systems, we need to be aware of the endianness convention used for the exchanged
data. More on this in the following chapters.

Using the address of pinned buffers
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

Pinned buffers have a fixed associated memory address. We can use the following
functions to read or write a mutable pinned buffer by using primitives for
``Addr#`` or ``Ptr``:

.. code:: haskell
   
   withBufferAddr# :: MonadIO m => Buffer 'Mutable 'Pinned fin heap -> (Addr# -> m a) -> m a
   withBufferPtr   :: MonadIO m => Buffer 'Mutable 'Pinned fin heap -> (Ptr b -> m a) -> m a

Similarly we can do the same thing with immutable buffers with the following
functions:

.. code:: haskell
   
   unsafeWithBufferAddr# :: MonadIO m => Buffer mut 'Pinned fin heap -> (Addr# -> m a) -> m a
   unsafeWithBufferPtr   :: MonadIO m => Buffer mut 'Pinned fin heap -> (Ptr b -> m a) -> m a

The difference in this case is that we mustn't use the memory writing primitives
of ``Addr#`` and ``Ptr`` when the buffer is immutable as it would break
referential transparency, hence the "unsafe" prefix.

------------------------------------------------------------------------------
Copy
------------------------------------------------------------------------------

We can copy data from one buffer to another with:

.. code:: haskell

   copyBuffer ::
      MonadIO m
      => Buffer mut pin0 fin0 heap0        -- ^ Source buffer
      -> Word                              -- ^ Offset in source buffer
      -> Buffer 'Mutable pin1 fin1 heap1   -- ^ Target buffer
      -> Word                              -- ^ Offset in target buffer
      -> Word                              -- ^ Number of Word8 to copy
      -> m ()

------------------------------------------------------------------------------
Performance
------------------------------------------------------------------------------

To enhance performance of the code using ``Buffer``, most functions have been
specialized with the ``SPECIALIZE INLINE`` pragma so that if your code uses a
specific buffer type (e.g. ``BufferI``, ``BufferM``...) it is as if the
generic ``Buffer`` GADT didn't exist.

You can use it in your own generic code. Example:

.. code:: haskell

   {-# SPECIALIZE INLINE bufferReadWord8IO :: MonadIO m => BufferI  -> Word -> m Word8 #-}
   {-# SPECIALIZE INLINE bufferReadWord8IO :: MonadIO m => BufferP  -> Word -> m Word8 #-}
