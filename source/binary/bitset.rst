==============================================================================
Bit sets (or "flags")
==============================================================================

We often use flags that are combined in a single word. Each flag is associated
to a bit of the word: if the bit is set the flag is active, otherwise the flag
isn't active.

``haskus-binary`` uses the ``CBitSet`` class to get the bit offset of each flag.
By default, it uses the Enum instance to get the bit offsets as in the following
example:

.. code:: haskell

   {-# LANGUAGE DeriveAnyClass #-}
   
   import Haskus.Format.Binary.BitSet
   
   data Flag
      = FlagX  -- bit 0
      | FlagY  -- bit 1
      | FlagZ  -- bit 2
      deriving (Show,Eq,Enum,CBitSet)

If you want to use different bit offsets, you can define your own CBitSet
instance:

.. code:: haskell

   -- Add 1 to the enum number to get the valid bit offset
   instance CBitSet Flag where
      toBitOffset   = (+1) . fromEnum
      fromBitOffset = toEnum . (\x -> x-1)


To use a bit set as a field in a structure, use BitSet:


.. code:: haskell

   data StructZ = StructZ
      { zField0 :: ...
      , zField1 :: BitSet Word32 Flag
      } deriving (Show,Generic,Storable)

The first type parameter of BitSet indicates the backing word type (i.e. the
size of the field in the structure). For instance, you can use Word8, Word16,
Word32 and Word64.

Use the following methods to manipulate the BitSet:

.. code:: haskell

   fromBits     :: (CBitSet a, FiniteBits b) => b -> BitSet b a
   toBits       :: (CBitSet a, FiniteBits b) => BitSet b a -> b
   member       :: (CBitSet a, FiniteBits b) => BitSet b a -> a -> Bool
   notMember    :: (CBitSet a, FiniteBits b) => BitSet b a -> a -> Bool
   toList       :: (CBitSet a, FiniteBits b) => BitSet b a -> [a]
   fromList     :: (CBitSet a, FiniteBits b, Foldable m) => m a -> BitSet b a
   intersection :: FiniteBits b => BitSet b a -> BitSet b a -> BitSet b a
   union        :: FiniteBits b => BitSet b a -> BitSet b a -> BitSet b a
 
Note that we don't check if bit offsets are outside of the backing word. You
have to choose a backing word that is large enough.


