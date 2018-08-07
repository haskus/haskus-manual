==============================================================================
Updating variants
==============================================================================

.. _mapVariant:

------------------------------------------------------------------------------
Mapping a single type: ``mapVariant``
------------------------------------------------------------------------------

We can easily apply a function ``f :: A -> B`` to a variant so that its value
type ``A`` is replaced with ``B``. If the value in the variant has type ``A``,
then ``f`` is applied to it to get the new value. Example:

.. code::

   
   x,y :: V '[String,Int]
   x = V "test"
   y = V @Int 10
   
   -- > mapVariant ((+5) :: Int -> Int) x
   -- V @String "test"

   -- > mapVariant ((+5) :: Int -> Int) y
   -- V @Int 15

Note that the resulting variant may contain the same type more than once. To
avoid this, we can either use :ref:`nubVariant <nubVariant>` or directly use
``mapNubVariant``:

.. code::

   -- > :t mapVariant (length :: String -> Int) x
   -- mapVariant (length :: String -> Int) x :: V '[Int, Int]

   -- > :t mapNubVariant (length :: String -> Int) x
   -- mapNubVariant (length :: String -> Int) x :: V '[Int]

   -- > mapNubVariant (length :: String -> Int) x
   -- V @Int 4

Generic code can be written with the ``MapVariant a b cs`` constraint and the
``ReplaceAll`` type family so that: ``mapVariant :: MapVariant a b cs => (a ->
b) -> V cs -> V (ReplaceAll a b cs)``

.. _mapVariantAt:

------------------------------------------------------------------------------
Mapping by index: ``mapVariantAt``
------------------------------------------------------------------------------

If we know the index of the value type we want to map, we can use
``mapVariantAt``. Example:

.. code::

   x,y :: V '[String,Int]
   x = V "test"
   y = V @Int 10

   -- > mapVariantAt @0 length x
   -- V @Int 4

   -- > mapVariantAt @0 length y
   -- V @Int 10

   -- > mapVariantAt @1 (+5) y
   -- V @Int 15

   -- > mapVariantAt @1 (+5) x
   -- V @[Char] "test"

Note that the compiler uses the type of the element whose index is given as
first argument to infer the type of the functions ``length`` and ``+5``, hence
we don't need type ascriptions.

We can use ``mapVariantAtM`` to perform an applicative (or monadic) update. For
example:

.. code::

   add :: Int -> Int -> IO Integer
   add a b = do
      putStrLn "Converting the result into Integer!"
      return (fromIntegral a + fromIntegral b)

   -- > mapVariantAtM @1 (add 5) x
   -- V @[Char] "test"

   -- > mapVariantAtM @1 (add 5) y
   -- Converting the result into Integer!
   -- V @Integer 15

------------------------------------------------------------------------------
Mapping only the first matching type: ``mapVariantFirst``
------------------------------------------------------------------------------

A variant can have the same type more than once in its value type list.
:ref:`mapVariant <mapVariant>` updates all the matching types in the list but
sometimes that's not what we want. We can use :ref:`mapVariantAt <mapVariantAt>`
if we know the index of the type we want to update. We can also use
``mapVariantFirst`` as follows if we want to update only the first matching
type:

.. code::

   vv :: V '[Int,Int,Int]
   vv = toVariantAt @1 5

   -- > r0 = mapVariant (show :: Int -> String) vv
   -- > r1 = mapVariantFirst (show :: Int -> String) vv

   -- > :t r0
   -- r0 :: V '[String,String,String]
   -- > :t r1
   -- r1 :: V '[String, Int, Int]
   -- 
   -- > r0
   -- V @[Char] "5"
   -- > r1
   -- V @Int 5

We can also apply an applicative (or monadic) function with
``mapVariantFirstM``:

.. code::

   printRetShow :: Show a => a -> IO String
   printRetShow a = do
      print a
      return (show a)

   -- > r2 = mapVariantFirstM (printRetShow @Int) vv
   -- > r2
   -- V @Int 5

   -- > :t r2
   -- r2 :: IO (V '[String, Int, Int])


------------------------------------------------------------------------------
TODO
------------------------------------------------------------------------------

* foldMapVariantFirst[M]
* foldMapVariant
* alterVariant
* traverseVariant
