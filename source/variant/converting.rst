==============================================================================
Converting variants
==============================================================================

------------------------------------------------------------------------------
Converting from/to a value
------------------------------------------------------------------------------

We can easily convert between a variant with a single value type and this value
type with ``variantToValue`` and ``variantFromValue``:

.. code::

   intV :: V '[Int]
   intV = V @Int 10

   > variantToValue intV
   10

   > :t variantToValue intV
   variantToValue intV :: Int

   > :t variantFromValue "Test"
   variantFromValue "Test" :: V '[String]

``variantFromValue`` is especially useful to avoid having to define the value
types of the variant explicitly.

------------------------------------------------------------------------------
Converting from/to Either
------------------------------------------------------------------------------

``variantFromEither`` and ``variantToEither`` can be used to convert between a
variant of arity 2 and the ``Either`` data type:

.. code::

   eith :: Either Int String
   eith = Left 10

   > :t variantFromEither eith
   variantFromEither eith :: V '[String, Int]

   x,y :: V '[String,Int]
   x = V "test"
   y = V @Int 10

   > variantToEither x
   Right "test"

   > variantToEither y
   Left 10

------------------------------------------------------------------------------
Extending
------------------------------------------------------------------------------

We can extend the value types of a variant by appending or prepending a list of
types with ``appendVariant`` and ``prependVariant``:

.. code::

   x :: V '[String,Int]
   x = V "test"

   data A = A
   data B = B

   px = prependVariant @'[A,B] x
   ax = appendVariant @'[A,B] x

   > :t ax
   ax :: V '[String, Int, A, B]

   > :t px
   px :: V '[A, B, String, Int]

You can use the ``Concat`` type family to specify the type of a concatened
variant:

.. code::

   data Error0 = Error0 deriving Show
   data Error1 = Error1 deriving Show

   checkErr ::
      ( Int :< is
      , os ~ Concat is '[Error0,Error1]
      , Error0 :< os
      , Error1 :< os
      ) => V is -> V os
   checkErr = \case
      V (0 :: Int) -> V Error0
      V (1 :: Int) -> V Error1
      v            -> appendVariant @'[Error0,Error1] v

   > checkErr (V @Int 0 :: V '[Float,Int])
   V @Error0 Error0

   > checkErr (V @Int 1 :: V '[Float,Int])
   V @Error1 Error1

   > checkErr (V @Int 2 :: V '[Float,Int])
   V @Int 2

   > checkErr (V @Float 5.0 :: V '[Float,Int])
   V @Float 5.0

   > z = checkErr (V @Float 5.0 :: V '[Float,Int,String,Double])
   > :t z
   z :: V '[Float, Int, [Char], Double, Error0, Error1]

Appending and prepending are very cheap operations: appending just messes with
types and performs nothing at runtime; prepending only increases the tag value
at runtime by a constant number.

------------------------------------------------------------------------------
Extending and reordering: lifting
------------------------------------------------------------------------------

We can extend and reorder the value types of a variant with the ``liftVariant``
function:

.. code::

   x :: V '[String,Int]
   x = V "test"

   -- adding Double and Float, and reordering
   y :: V '[Double,Int,Float,String]
   y = liftVariant x

You can use the ``Liftable is os`` constraint to write generic code and to
ensure that the type list ``is`` is a subset of ``os``:

.. code::

   liftX :: (Liftable is (Double ': Float ': is))
         => V is -> V (Double ': Float ': is)
   liftX = liftVariant

   > :t liftX x
   liftX x :: V '[Double, Float, String, Int]
   
   > :t liftX (V "test" :: V '[String])
   liftX (V "test" :: V '[String]) :: V '[Double, Float, String]


.. _nubVariant:

------------------------------------------------------------------------------
Nubing
------------------------------------------------------------------------------

If the list of value types of a variant contains the same type more than once,
we can decide to only keep one of them with ``nubVariant``:

.. code::

   > z = nubVariant (V "test" :: V '[String,Int,Double,Float,Double,String])
   > :t z
   z :: V '[String, Int, Double, Float]

You can use the ``Nub`` type family to write generic code.


------------------------------------------------------------------------------
Flattening
------------------------------------------------------------------------------

If the value types of a variant are themselves variants, you can flatten them
with ``flattenVariant``:

.. code::

   x :: V '[String,Int]
   x = V "test"

   nest :: V '[ V '[String,Int], V '[Float,Double]]
   nest = V x

   > :t flattenVariant nest
   flattenVariant nest :: V '[String, Int, Float, Double]

You can use the ``Flattenable`` type-class and the ``FlattenVariant`` type
family to write generic code.

------------------------------------------------------------------------------
Joining
------------------------------------------------------------------------------

We can transform a variant of functor values (e.g., ``V '[m a, m b, m c]``) into
a single functor value (e.g., ``m (V '[a,b,c])``) with ``joinVariant``:

.. code::

   fs0,fs1,fs2 :: V '[ Maybe Int, Maybe String, Maybe Double]
   fs0 = V @(Maybe Int) (Just 10)
   fs1 = V (Just "Test")
   fs2 = V @(Maybe Double) Nothing

   > joinVariant @Maybe fs0
   Just (V @Int 10)

   > joinVariant @Maybe fs1
   Just (V @[Char] "Test")

   > joinVariant @Maybe fs2
   Nothing


It also works with ``IO`` for example:

.. code::

   printRet :: Show a => a -> IO a
   printRet a = do
      print a
      return a

   ms0,ms1 :: V '[ IO Int, IO String, IO Double]
   ms0 = V @(IO Int) (printRet 10)
   ms1 = V (printRet "Test")

   > joinVariant @IO ms0
   10
   V @Int 10

   > joinVariant @IO ms1
   "Test"
   V @[Char] "Test"

   > :t joinVariant @IO ms0
   joinVariant @IO ms0 :: IO (V '[Int, String, Double])

Writing generic code requires the use of the ``JoinVariant m xs`` constraint and
the resulting list of value types can be obtained with the ``ExtractM m xs``
type family.

.. code::

   > :t joinVariant
   joinVariant :: JoinVariant m xs => V xs -> m (V (ExtractM m xs))


.. note::
   
   With ``IO`` it is possible to use the ``joinVariantUnsafe`` function which
   doesn't require the type application and doesn't use the ``JoinVariant``
   type-class. However some other functor types aren't supported (e.g.,
   ``Maybe``) and using ``joinVariantUnsafe`` with them makes the program crash
   at runtime.

------------------------------------------------------------------------------
Combining two variants (product)
------------------------------------------------------------------------------

We can combine two variants into a single variant containing a tuple with
``productVariant``:

.. code::

   fl :: V '[Float,Double]
   fl = V @Float 5.0

   d :: V '[Int,Word]
   d = V @Word 10

   dfl = productVariant d fl

   > dfl
   V @(Word,Float) (10,5.0)

   > :t dfl
   dfl :: V '[(Int, Float), (Int, Double), (Word, Float), (Word, Double)]

------------------------------------------------------------------------------
Converting to tuple/HList
------------------------------------------------------------------------------

variantToTuple
variantToHList
