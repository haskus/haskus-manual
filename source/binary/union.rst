==============================================================================
Unions
==============================================================================

An union provides several ways to access the same buffer of memory. To use them
with ``haskus-binary``, you need to give the list of available representations
in a type as follows:

.. code:: haskell

   {-# LANGUAGE DeriveAnyClass #-}
   {-# LANGUAGE DataKinds #-}
   
   import Haskus.Format.Binary.Union
   
   u :: Union '[Word8, Word64, Vector 5 Word16]

Unions are storable so you can use them as fields in storable structures or
you can directly ``peek``/``poke`` them.

You can retrieve a member of the union with ``fromUnion``.  The extracted type
must be a member of the union otherwise it won't compile.

.. code:: haskell

   fromUnion u :: Word64
   fromUnion u :: Word8
   fromUnion u :: Vector 5 Word16
   fromUnion u :: Word32 -- won't compile!

To create a new union from one of its member, use ``toUnion`` or ``toUnionZero``.
The latter sets the remaining bytes of the buffer to 0. In the example, the union
uses 10 bytes (5 * 2 for Vector 5 Word16) and we write 8 bytes (sizeOf Word64)
hence there are two bytes that can be left uninitialized (toUnion) or set to 0
(toUnionZero).

.. code:: haskell

   u :: Union '[Word8,Word64,Vector 5 Word16]
   u = toUnion (0x1122334455667788 :: Word64)
   
   > print (fromUnion u :: Vector 5 Word16)
   fromList [30600,21862,13124,4386,49850]
   
   -- or
   u = toUnionZero (0x1122334455667788 :: Word64)
   > print (fromUnion u :: Vector 5 Word16)
   fromList [30600,21862,13124,4386,0]
