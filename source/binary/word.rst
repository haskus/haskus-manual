==============================================================================
Word, Int
==============================================================================

The `Word module <http://github.com/haskus/haskus-binary/tree/master/src/lib/Haskus/Format/Binary/Word.hs>`_ contains data
types representing unsigned words (Word8, Word16, Word32, etc.) and signed
integers (Int8, Int16, Int32, etc.). It also contains some C types such as
CSize, CShort, CUShort, CLong, CULong, etc.


------------------------------------------------------------------------------
Endianness
------------------------------------------------------------------------------

Words and Ints are stored (i.e., read and written) using host endianness (byte
ordering). ``AsBigEndian`` and ``AsLittleEndian`` data types in the `Endianness
module
<http://github.com/haskus/haskus-binary/tree/master/src/lib/Haskus/Format/Binary/Endianness.hs>`_
allow you to force a different endianness.

The following example shows a data type containing a field for each endianness
variant. We explain how to use this kind of data type as a C structure later in
this document.

.. code:: haskell

   data Dummy = Dummy
      { fieldX :: Word32                -- ^ 32-byte unsigned word (host endianness)
      , fieldY :: AsBigEndian Word32    -- ^ 32-byte unsigned word (big-endian)
      , fieldZ :: AsLittleEndian Word32 -- ^ 32-byte unsigned word (little-endian)
      } deriving (Generic,Storable)


We can also explicitly change the endianness with the following methods:
``hostToBigEndian``, ``hostToLittleEndian``, ``bigEndianToHost``,
``littleEndianToHost``, ``reverseBytes``.

Each of these methods is either equivalent to ``id`` or to ``reverseBytes``
depending on the host endianness.


