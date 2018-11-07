.. _variant_safe_pattern_matching:

==============================================================================
Safe pattern matching
==============================================================================

Suppose we have the following variants:

.. code:: haskell

   x,y,z :: V '[String,Int,Float]
   x = V "test"
   y = V @Int 10
   z = V @Float 5.0


------------------------------------------------------------------------------
Unsafe pattern-matching
------------------------------------------------------------------------------

We can use pattern matching on the constructors:

.. code:: haskell

   printV' :: V '[String,Int,Float] -> IO ()
   printV' = \case
      V (s :: String) -> putStrLn ("Found string: " ++ s)
      V (i :: Int)    -> putStrLn ("Found int: " ++ show i)
      V (f :: Float)  -> putStrLn ("Found float: " ++ show f)
      _               -> undefined

   > printV' x
   Found string: test
   > printV' y
   Found int: 10
   > printV' z
   Found float: 5.0

However the compiler cannot detect that the pattern matching is complete, hence
we have the choice between a warning or adding a wildcard match as we have done
above.

------------------------------------------------------------------------------
Safe pattern-matching with continuations
------------------------------------------------------------------------------

Another solution is to rely on multi-continuations. Then we can provide a
function per constructor as in a pattern-matching. For instance, with
multi-continuations we can transform a variant ``V '[A,B,C]`` into a function
whose type is ``(A -> r, B -> r, C -> r) -> r``. Hence the compiler will ensure
that we provide the correct number of alternatives in the continuation tuple
(the first parameter).

Transforming a Variant into a multi-continuation is done with ``variantToCont``.
Mapping the continuation tuple is done with ``>::>``.

.. code:: haskell

   import Haskus.Utils.ContFlow

   printV :: V '[String,Int,Float] -> IO ()
   printV v = variantToCont v >::>
      ( \s -> putStrLn ("Found string: " ++ s)
      , \i -> putStrLn ("Found int: " ++ show i)
      , \f -> putStrLn ("Found float: " ++ show f)
      )

------------------------------------------------------------------------------
Unordered continuations (``>:%:>``)
------------------------------------------------------------------------------

By using the ``>:%:>`` operator instead of ``>::>``, we can provide
continuations in any order as long as an alternative for each constructor is
provided.

The types must be unambiguous as the Variant constructor types can't be used to
infer the continuation types (as is done with ``>::>``). Hence the type
ascriptions in the following example:

.. code:: haskell

   printU :: V '[String,Int,Float] -> IO ()
   printU v = variantToCont v >:%:>
      ( \f -> putStrLn ("Found float: " ++ show (f :: Float))
      , \s -> putStrLn ("Found string: " ++ s)
      , \i -> putStrLn ("Found int: " ++ show (i :: Int))
      )

------------------------------------------------------------------------------
Splitting constructors
------------------------------------------------------------------------------

We can chose to handle only a subset of the values of a Variant by using
``splitVariant``.

For instance in the following example we only handle ``Int`` and ``Float``
values. The other ones are considered as left-overs:

.. code::

   printNum v = case splitVariant @'[Float,Int] v of
      Right v -> variantToCont v >:%:>
         ( \f -> putStrLn ("Found float: " ++ show (f :: Float))
         , \i -> putStrLn ("Found int: " ++ show (i :: Int))
         )
      Left leftovers -> putStrLn "Not a supported number!"

   > printNum x
   Not a supported number!
   > printNum y
   Found int: 10
   > printNum z
   Found float: 5.0

The code is generic and can be used with any Variant type:

.. code:: haskell

   w,k,u :: V '[String,Int,Double,Maybe Int]
   w = V @Double 1.0
   k = V (Just @Int 10)
   u = V @Int 17

   > printNum w
   Not a supported number!
   > printNum k
   Not a supported number!
   > printNum u
   Found int: 17
