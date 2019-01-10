==============================================================================
Bit fields
==============================================================================

You may need to define bit fields over words. For instance, you can
have a Word16 split into 3 fields X, Y and Z composed of 5, 9 and 2 bits
respectively.

+-------------+-----------+-------------------+-----+
|             | X         | Y                 | Z   |
+-------------+-----------+-------------------+-----+
| w :: Word16 | 0 0 0 0 0 | 0 0 0 0 0 0 0 0 0 | 0 0 |
+-------------+-----------+-------------------+-----+


You define it as follows:

.. code:: haskell

   {-# LANGUAGE DataKinds #-}
   {-# LANGUAGE TypeApplications #-}
   
   import Haskus.Format.Binary.BitField
   
   w :: BitFields Word16 '[ BitField 5 "X" Word8 
                          , BitField 9 "Y" Word16
                          , BitField 2 "Z" Word8
                          ]
   w = BitFields 0x0102

Note that each field has its own associated type (e.g. Word8 for X and Z)
that must be large enough to hold the number of bits for the field.

Operations on BitFields expect that the cumulated size of the fields is equal
to the whole word size: use a padding field if necessary.

You can extract and update the value of a field by its name:

.. code:: haskell

   x = extractField @"X" w
   z = extractField @"Z" w
   w' = updateField @"Y" 0x100 w
   -- w' = 0x402
   
   z = extractField @"XXX" w -- won't compile
   
   w'' = withField @"Y" (+2) w

Fields can also be 'BitSet' or 'EnumField':

.. code:: haskell

   {-# LANGUAGE DataKinds #-}
   {-# LANGUAGE DeriveAnyClass #-}
   
   import Haskus.Format.Binary.BitField
   import Haskus.Format.Binary.Enum
   import Haskus.Format.Binary.BitSet
   
   data A = A0 | A1 | A2 | A3 deriving (Show,Enum,CEnum)
   
   data B = B0 | B1 deriving (Show,Enum,CBitSet)
   
   w :: BitFields Word16 '[ BitField 5 "X" (EnumField Word8 A)
                          , BitField 9 "Y" Word16
                          , BitField 2 "Z" (BitSet Word8 B)
                          ]
   w = BitFields 0x1503

BitFields are storable and can be used in storable structures.

You can easily pattern-match on all the fields at the same time with
``matchFields`` and ``matchNamedFields``. It creates a tuple containing one value
(and its name with ``matchNamedFields``) per field.

.. code:: haskell

   > matchFields w
   (EnumField A2,320,fromList [B0,B1])
   
   > matchNamedFields  w
   (("X",EnumField A2),("Y",320),("Z",fromList [B0,B1]))

