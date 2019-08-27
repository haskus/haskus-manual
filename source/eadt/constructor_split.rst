------------------------------------------------------------------------------
Splitting constructors
------------------------------------------------------------------------------

We can chose to handle only a subset of the constructors of an EADT by using
``splitVariantF``.

For instance in the following example we only handle ``EvenF Int`` and ``OddF Int``
constructors. The other ones are considered as left-overs:

.. code::

   alg x = case splitVariantF @'[EvenF Int, OddF Int] x of
      Right v        -> v >:>
                           ( \(EvenF a l) -> "Even : " ++ l
                           , \(OddF a l)  -> "Odd : " ++ l
                           )
      Left leftovers -> "something else"

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

