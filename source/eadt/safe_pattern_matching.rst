.. _eadt_safe_pattern_matching:

==============================================================================
Safe pattern matching
==============================================================================

Suppose we have the following ``List`` EADT:

.. code:: haskell

   data ConsF a l = ConsF a l deriving (Functor)
   data NilF    l = NilF      deriving (Functor)

   eadtPat 'ConsF "Cons"
   eadtPat 'NilF  "Nil"

   type List a = EADT '[ConsF a, NilF]

   -- pattern for a specific EADT: List a
   pattern ConsList :: a -> List a -> List a
   pattern ConsList a l = Cons a l


We can use pattern matching on ``List`` constructors:

.. code:: haskell

   showEADTList :: Show a => List a -> String
   showEADTList = \case
      ConsList a l -> show a ++ " : " ++ showEADTList l
      Nil          -> "Nil"
      _            -> undefined

   > putStrLn (showEADTList intList)
   10 : 20 : 30 : Nil


However the compiler cannot detect that the pattern matching is complete, hence
we have the choice between a warning or adding a wildcard match as we have done
above.

Another solution is to rely on multi-continuations. Then we can provide a
function per constructor as in a pattern-matching. For instance, with
multi-continuations we can transform an ``EADT '[A,B,C]`` into a function whose
type is ``(A -> r, B -> r, C -> r) -> r``. Hence the compiler will ensure that
we provide the correct number of alternatives in the continuation tuple (the
first parameter).

------------------------------------------------------------------------------
Explicitly recursive example
------------------------------------------------------------------------------

Transforming an EADT into a multi-continuation is done with ``eadtToCont``.
Mapping the continuation tuple is done with ``>::>``.

.. code:: haskell

   import Haskus.Utils.ContFlow

   showCont' l = eadtToCont l >::>
      ( \(ConsF a r) -> show a ++ " : " ++ showCont' r -- explicit recursion
      , \NilF        -> "Nil"
      )

   > showCont' intList
   "10 : 20 : 30 : Nil"

------------------------------------------------------------------------------
Catamorphism example
------------------------------------------------------------------------------

Transforming a ``VariantF`` into a multi-continuation is done with
``variantFToCont``. It can be useful with :ref:`recursion schemes
<eadt_recursion_schemes>`.


.. code:: haskell

   import Haskus.Utils.ContFlow

   showCont l = variantFToCont l >::>
      ( \(ConsF a r) -> show a ++ " : " ++ r -- no explicit recursion
      , \NilF        -> "Nil"
      )

   > cata showCont intList
   "10 : 20 : 30 : Nil"

------------------------------------------------------------------------------
Transformation example
------------------------------------------------------------------------------

We can use this approach to transform EADT. For instance list mapping:

.. code:: haskell

   import Haskus.Utils.ContFlow

   mapList f l = variantFToCont l >::>
      ( \(ConsF a r) -> Cons (f a) r
      , \NilF        -> Nil
      )

   > eadtShow (cata (mapList (+5)) intList :: List Int)
   "15 : 25 : 35 : Nil" 

We can also transform an EADT into another EADT:

.. code:: haskell

   -- Some new Even and Odd constructors
   data EvenF a l = EvenF a l deriving (Functor)
   data OddF a l  = OddF a l deriving (Functor)

   eadtPat 'EvenF "Even"
   eadtPat 'OddF  "Odd"

   instance (Show a) => MyShow' (EvenF a) where
      myShow' (EvenF a l) = show a ++ " {even} : " ++ l

   instance (Show a) => MyShow' (OddF a) where
      myShow' (OddF a l) = show a ++ " {odd} : " ++ l

   
   -- convert Cons constructor into Odd or Even constructor, depending on the
   -- cell value
   evenOdd l = variantFToCont l >::>
      ( \(ConsF a r) -> if even a then Even a r
                                  else Odd  a r
      , \NilF        -> Nil
      )

   intList' :: List Int
   intList' = Cons (3 :: Int) $ Cons (4 :: Int) $ Cons (5 :: Int) Nil

   > eadtShow (cata evenOdd intList' :: EADT '[EvenF Int, OddF Int, NilF])
   "3 {odd} : 4 {even} : 5 {odd} : Nil"

------------------------------------------------------------------------------
Splitting constructors
------------------------------------------------------------------------------

We can chose to handle only a subset of the constructors of an EADT by using
``splitVariantF``.

For instance in the following example we only handle ``EvenF Int`` and ``OddF Int``
constructors. The other ones are considered as left-overs:

.. code::

   alg x = case splitVariantF @'[EvenF Int, OddF Int] x of
      Left v          -> variantFToCont v >::>
                           ( \(EvenF a l) -> "Even : " ++ l
                           , \(OddF a l)  -> "Odd : " ++ l
                           )
      Right leftovers -> "something else"

We can test this code with:

.. code:: haskell

   eo :: EADT '[EvenF Int, OddF Int, NilF]
   eo = cata evenOdd intList'

   eo2 :: EADT '[ConsF Int, EvenF Int, OddF Int, NilF]
   eo2 = Even (10 :: Int) $ Odd (5 :: Int) $ Cons (7 :: Int) $ Odd (7 :: Int) Nil

   > cata alg eo
   "Odd : Even : Odd : something else"

   > cata alg eo2
   "Even : Odd : something else"

Note that the traversal ends when it encounters an unhandled constructor.

------------------------------------------------------------------------------
Unordered continuations (``>:%:>``)
------------------------------------------------------------------------------

By using the ``>:%:>`` operator instead of ``>::>``, we can provide
continuations in any order as long as an alternative for each constructor is
provided.

The types must be unambiguous as the EADT type can't be used to infer the
continuation types (as is done with ``>::>``). Hence the type ascriptions in the
following example:

.. code:: haskell

   showCont'' l = eadtToCont l >:%:>
      ( \(NilF :: NilF (List Int)) -> "Nil"
      , \(ConsF a r)               -> show (a :: Int) ++ " : " ++ showCont'' r
      )

   > showCont'' intList
   "10 : 20 : 30 : Nil"
