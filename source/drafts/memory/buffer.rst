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
``Finalized`` explained below.

A finalizer is just a side-effecting function that can be associated to a data.
When a data is to be collected by GHC's garbage collector, GHC executes its
associated finalizers if the data has some before releasing the data.  This
mechanism is especially useful in the domain of explicit memory management (e.g.
to release externally allocated memory), hence ``Buffers`` can be ``Finalized``
to ease the use of this mechanism. In particular it ensures execution order of
the finalizers.
