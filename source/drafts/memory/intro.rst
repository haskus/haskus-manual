==============================================================================
Introduction
==============================================================================

This document is about explicit ("manual") memory management in Haskell. It
starts from the very low level memory allocation and buffer management and then
builds up to "pointer" management, binary writer/reader monads, binary
serialization of Haskell data, large file mapping, embedding of large binary
blobs into applications, etc.


------------------------------------------------------------------------------
Rationale
------------------------------------------------------------------------------

Haskell is a garbage-collected language and is known for its use of immutable data: why would we like to manage memory explicitly? Here is a list of reasons:

1. Support for large data sets
2. Close to the metal: better control and usually faster operations
3. Interaction with the outside world
4. Live outside of the garbage-collector (no GC overhead)

------------------------------------------------------------------------------
Memory model
------------------------------------------------------------------------------

The memory model we consider is the `flat memory model
<https://en.wikipedia.org/wiki/Flat_memory_model>`_. A memory is an array of
cells each containing one **bit**. A bit is a binary digit (either 0 or 1).
Instead of indexing each bit individually, the memory is divided in coarser
groups of bits. Nowadays most architectures pack bits in groups of 8 bits
(called "byte" or "Word8" in Haskell) so that's what we assume.

Consecutive bytes are assigned consecutive addresses: an **address** is a number
(starting from 0) which maximal value depends on the architectures. For example,
on X86_64 architectures an address is stored in a 64-bit word but only the 48
lower bits are used.

Even if the whole memory is addressable, some addresses can't be given as
parameters to reading/writing instructions depending on the execution context
(state of the memory, state of the processes, etc.). Otherwise an
error/exception/interruption may be triggered or the result may be undefined.

This memory model is abstract: it can be applied to physical memory or to
virtual memory managed by some software (operating systems, system libraries,
etc.).

.. _memory_allocation:

------------------------------------------------------------------------------
Memory allocation
------------------------------------------------------------------------------

Nowadays most mainstream systems and architectures require the use of a memory
allocator. Applications can't just read or write random addresses in memory
otherwise it could lead to data corruption or to prohibited access errors.
Instead programs must reclaim a chunk of consecutive cells (i.e. a
**buffer**) to the memory allocator.

.. note::

   A memory allocator is also abstract: its facilities can be provided by the
   hardware (less common), by the operating system (usually not used directly)
   or by some "system libraries" built on the the operating system allocator
   (most common).

The memory allocator tries to find a suitable set of consecutive memory cells
that fulfills the given constraints (minimal number of cells, address constraints
such as alignment or maximal value, etc.). It makes sure that these cells are
now accessible to the calling code and returns the address of the first cell to
the calling code.

Programs can release buffers that are no longer used. Memory cells of released
buffers can be used by the memory allocator for future memory allocations.

Allocating and releasing buffers can lead to memory `fragmentation
<https://en.wikipedia.org/wiki/Fragmentation_(computing)>`_: there are enough
free memory cells to allocate but these are not consecutive because allocated
memory cells are still alive between them.

.. _memory_allocation_ghc:

------------------------------------------------------------------------------
GHC memory allocation
------------------------------------------------------------------------------

GHC provides its own memory allocator on top of the system one. It is well
integrated with the copying garbage collector.

The main difference with the system one is that there is an additional
indirection: instead of returning the address of the first allocated memory
cell, GHC's memory allocator returns an abstract object that contains a mutable
memory address. GHC can freely copy the contents of the buffer to another
location and update the address in the abstract object. This avoids
fragmentation at the cost of buffer copying.

As copying buffers can be too costly, GHC's memory allocator also supports the
allocation of **pinned** buffers: these buffers are guaranteed not to be moved,
exactly like system allocated buffers. As an optimization, GHC automatically
considers large buffers as pinned buffers.

GHC also distinguishes at the type level buffers whose cells are mutable or
immutable. Immutable buffers can shared, duplicated at will, etc. Mutable
buffers are cheaper to use as we don't have to duplicate the whole buffer to
change one cell as it would be the case with immutable buffers. Immutable
buffers are created by "freezing" mutable buffers: by convention a frozen
mutable buffer mustn't be used anymore (linear types may help to ensure this
statically in the future).

GHC allocated buffers are automatically released by the garbage collector.

.. note::

   Memory allocation in GHC is done with ``MutableByteArray#`` and
   ``ByteArray#`` data types and their primitives.
