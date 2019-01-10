==============================================================================
Structures (records)
==============================================================================

You map C data structures with Haskell data type as follows:

.. code:: haskell

   {-# LANGUAGE DeriveAnyClass #-}
   {-# LANGUAGE DeriveGeneric #-}
   
   import Haskus.Format.Binary.Storable
   import Haskus.Utils.Types.Generics (Generic)
   
   data StructX = StructX
      { xField0 :: Word8
      , xField1 :: Word64
      } deriving (Show,Generic,Storable)


The Storable instance handles the alignment of the field as a C non-packed
structure would (i.e. there are 7 padding bytes between ``xField0`` and
``xField1``).

``peek`` and ``poke`` can be used to read and write the data structure in memory.

------------------------------------------------------------------------------
Nesting
------------------------------------------------------------------------------

Data structures can be nested:

.. code:: haskell

   data StructY = StructY
      { yField0 :: StructX
      , yField1 :: Word64
      } deriving (Show,Generic,Storable)


