.. _buffer_view:

==============================================================================
Views
==============================================================================

Views can be used to refer to a subset of the cells of a buffer.

------------------------------------------------------------------------------
View patterns
------------------------------------------------------------------------------

The cells referred by a view may not be contiguous. Currently we support the
following patterns:

.. code:: haskell

   -- | A view pattern
   data ViewPattern
      = PatternFull  -- ^ The whole buffer
      | Pattern1D    -- ^ 1D slice
         { pattern1DOffset :: {-# UNPACK #-} !Word -- ^ Offset of the first cell
         , pattern1DSize   :: {-# UNPACK #-} !Word -- ^ Number of cells
         }
      | Pattern2D    -- ^ 2D slice
         { pattern2DOffset :: {-# UNPACK #-} !Word -- ^ Offset of the first line
         , pattern2DWidth  :: {-# UNPACK #-} !Word -- ^ Width (line size)
         , pattern2DHeight :: {-# UNPACK #-} !Word -- ^ Height (number of lines)
         , pattern2DStride :: {-# UNPACK #-} !Word -- ^ Stride (space between two lines)
         }
      | PatternOn ViewPattern ViewPattern -- ^ Composed pattern

------------------------------------------------------------------------------
View sources
------------------------------------------------------------------------------

A view can use one of the following sources: a (strong) buffer, a weak buffer or
a weak view. 

Strong views keep the underlying buffer alive, while weak views allow the source
to be garbage collected.

1. The source is a buffer. The view keeps the buffer alive

2. The source is a weak buffer. If the buffer is collected, its contents
   is copied in to a new buffer and the view is updated to use it.

3. The source is a weak view. If the source view is collected, the
   current view is updated to use whatever the source view uses as a
   source (another view or a buffer).
   This mechanism makes buffer contents cascade into smaller views while
   preserving some sharing.


------------------------------------------------------------------------------
Weak views
------------------------------------------------------------------------------

Weak views are used so that the underlying buffer can be freed by the GC.
When it happens and if the view is still alive the contents of the buffer
used by the view is copied into a fresh (usually smaller) buffer.

Suppose we have a big buffer B. We can have buffer views on B, say vb1 and vb2.

.. code::

  B <----- vb1
  ^------- vb2

These views don't duplicate B's contents and they keep B alive.
If the views are much smaller than B, it may not be what we want: a lot of
space is wasted and we would better duplicate B's data required by the views
and free B.

To support this, we can use "weak buffer views", say wbv1 and wbv2.

.. code::

  B <~~~~~ wbv1
  ^~~~~~~~ wbv2

If/when B is collected, new buffers are created from it for the views:

.. code::

  B1 <----- wbv1
  B2 <----- wbv2

We can also create "weak view views", say wvv1 and wvv2:

.. code::

  B <~~~~~ wvb1 <~~~~~ wvv1
             ^~~~~~~~~ wvv2

If/when B is collected before wvb1, the sharing is kept while the required
contents of B is duplicated:

.. code::

  B' <---- wbv1 <~~~~~ wvv1
             ^~~~~~~~~ wvv2

When wbv1 is collected, we can be in one of the following state depending if
B has been collected already or not:

.. code::

  B <~~~~~~~~~~~~~~~~~ wvv1
  ^~~~~~~~~~~~~~~~~~~~ wvv2

             B' <~~~~~ wvv1
             ^~~~~~~~~ wvv2


------------------------------------------------------------------------------
Example
------------------------------------------------------------------------------

.. code:: haskell

   >>> :set -XOverloadedLists
   >>> import System.Mem
   >>> v <- newBufferWeakView ([10,11,12,13,14,15,16,17] :: BufferI) (Pattern1D 2 4)
   >>> v2 <- newViewWeakView v (Pattern1D 1 1)
   >>> putStr =<< showViewState v2
   View source: weak view
   Source size: 4
   View pattern: Pattern1D {pattern1DOffset = 1, pattern1DSize = 1}
   Wasted space: 75%
   Source:
      View source: weak buffer
      Source size: 8
      View pattern: Pattern1D {pattern1DOffset = 2, pattern1DSize = 4}
      Wasted space: 50%

   >>> performGC

   >>> putStr =<< showViewState v2
   View source: weak view
   Source size: 4
   View pattern: Pattern1D {pattern1DOffset = 1, pattern1DSize = 1}
   Wasted space: 75%
   Source:
      View source: buffer       -- the source of v (a weak buffer of size 8) has
      Source size: 4            -- been replaced by a strong buffer of size 4 when
      View pattern: PatternFull -- the source has been collected
      Wasted space: 0%


.. code:: haskell

   >>> v <- (`newViewWeakView` Pattern1D 1 2) =<< newBufferWeakView ([10,11,12,13,14,15,16,17] :: BufferI) (PatternFull)

   >>> putStr =<< showViewState v
   Source size: 8
   View pattern: Pattern1D {pattern1DOffset = 1, pattern1DSize = 2}
   Wasted space: 75%
   Source:
      View source: weak buffer
      Source size: 8
      View pattern: PatternFull
      Wasted space: 0%
   
   >>> performGC

   >>> putStr =<< showViewState v
   View source: buffer
   Source size: 2
   View pattern: PatternFull
   Wasted space: 0%

