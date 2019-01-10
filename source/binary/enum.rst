==============================================================================
Enums
==============================================================================

If you have a C enum (or a set of #define's) with consecutive values and
starting from 0, you can do:

.. code:: haskell

   {-# LANGUAGE DeriveAnyClass #-}
   
   import Haskus.Format.Binary.Enum
   
   data MyEnum
      = MyEnumX
      | MyEnumY
      | MyEnumZ
      deriving (Show,Eq,Enum,CEnum)


If the values are not consecutive or don't start from 0, you can write your own
``CEnum`` instance:

.. code:: haskell

   -- Add 1 to the enum number to get the valid value
   instance CEnum MyEnum where
      fromCEnum = (+1) . fromIntegral . fromEnum
      toCEnum   = toEnum . (\x -> x-1) . fromIntegral


To use an Enum as a field in a structure, use ``EnumField``:

.. code:: haskell

   data StructZ = StructZ
      { zField0 :: StructX
      , zField1 :: EnumField Word32 MyEnum
      } deriving (Show,Generic,Storable)


The first type parameter of ``EnumField`` indicates the backing word type (i.e. the
size of the field in the structure). For instance, you can use Word8, Word16,
Word32 and Word64.

To create or extract an ``EnumField``, use the methods:

.. code:: haskell

   fromEnumField :: CEnum a => EnumField b a -> a
   toEnumField   :: CEnum a => a -> EnumField b a


We use a CEnum class that is very similar to Enum because Enum is a special
class that has access to data constructor tags. If we redefine Enum, we cannot
use ``fromEnum`` to get the data constructor tag.


