.. _eadt_constructor_removal:

==============================================================================
Constructor removal
==============================================================================

Removing constructors from an EADT is equivalent to transforming every instance
of these constructors into other constructors of another EADT.

If we fully know the source and target EADTs, we can use
:ref:`safe pattern-matching <eadt_safe_pattern_matching>` as follows:

.. code::

   -- replace Even and Odd constructors with a Cons constructor
   removeOddEven l = variantFToCont l >::>
      (\(EvenF a r) -> Cons a r
      ,\(OddF  a r) -> Cons a r
      ,\NilF        -> Nil
      )

   eo :: EADT '[EvenF Int, OddF Int, NilF]
   eo = Even (10 :: Int) $ Odd (5 :: Int) $ Odd (7 :: Int) Nil

   > eadtShow (cata removeOddEven eo :: List Int)
   "3 : 4 : 5 : Nil"

Note that ``removeOddEven`` only works on a specific EADT. If we want it to work on
any EADT that contains ``Even`` and ``Odd`` constructors, read the following
section.

------------------------------------------------------------------------------
Generic constructor removal
------------------------------------------------------------------------------

If we want to be able to remove constructors from a generic EADT, we can use
type-classes with an overlappable instance handling the generic case (i.e. that
only transfers constructors from one EADT to another without modifying them).

Example of removing all the ``OddF a`` and ``EvenF a`` constructors by replacing
them with a single ``ConsF a`` constructor:

.. code:: haskell

   class RemoveOddEven ys (f :: * -> *) where
      removeOddEven :: f (EADT ys) -> EADT ys

   -- replace Odd and Even with Cons
   instance ConsF a :<: ys => RemoveOddEven ys (OddF a) where
      removeOddEven (OddF a l) = Cons a l 

   instance ConsF a :<: ys => RemoveOddEven ys (EvenF a) where
      removeOddEven (EvenF a l) = Cons a l 

   -- handle the combinator
   instance
      ( AlgVariantF (RemoveOddEven ys) (EADT ys) xs
      ) => RemoveOddEven ys (VariantF xs)
      where
         removeOddEven = algVariantF @(RemoveOddEven ys) removeOddEven

   -- handle remaining constructors
   instance {-# OVERLAPPABLE #-} f :<: ys => RemoveOddEven ys f where
      removeOddEven = VF -- keep the other constructors as is

We can test it:

.. code:: haskell

   eo :: EADT '[EvenF Int, OddF Int, NilF]
   eo = Even (10 :: Int) $ Odd (5 :: Int) $ Odd (7 :: Int) Nil

   > eadtShow (cata removeOddEven eo :: List Int)
   "3 : 4 : 5 : Nil"

   -- EADT with an additional `ConsF Int` constructor
   eo2 :: EADT '[ConsF Int, EvenF Int, OddF Int, NilF]
   eo2 = Even (10 :: Int) $ Odd (5 :: Int) $ Cons (7 :: Int) $ Odd (7 :: Int) Nil

   > eadtShow (cata removeOddEven eo2 :: List Int)
   "10 : 5 : 7 : 7 : Nil"

   -- EADT with an additional `ConsF String` constructor
   eo3 :: EADT '[ConsF Int, EvenF Int, OddF Int, ConsF String, NilF]
   eo3 = Even (10 :: Int) $ Cons "Test" $ Odd (5 :: Int) $ Cons (7 :: Int) $ Odd (7 :: Int) Nil

   > eadtShow (cata removeOddEven eo3 :: EADT '[ConsF Int, ConsF String, NilF])
   "10 : \"Test\" : 5 : 7 : 7 : Nil"


Note that we need to specify the resulting type as it could be anything
fulfilling the constraints.
