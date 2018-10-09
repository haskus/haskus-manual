.. _eadt_constructor_removal:

==============================================================================
Constructor removal
==============================================================================

Removing a constructor from an EADT is equivalent to transforming any instance
of this constructor (if any) into another constructor of another EADT.

To do that generically we can use type-classes with an overlappable instance
handling the generic case (i.e. that only transfers constructors from one EADT
to another without modifying them).


------------------------------------------------------------------------------
Example
------------------------------------------------------------------------------

For instance, we may want to remove all the ``OddF a`` and ``EvenF a``
constructors by replacing them with a single ``ConsF a`` constructor. We can do
it as follow:

.. code:: haskell

   class RemoveOddEven ys (f :: * -> *) where
      removeOE :: f (EADT ys) -> EADT ys

   -- replace Odd and Even with Cons
   instance ConsF a :<: ys => RemoveOddEven ys (OddF a) where
      removeOE (OddF a l) = Cons a l 

   instance ConsF a :<: ys => RemoveOddEven ys (EvenF a) where
      removeOE (EvenF a l) = Cons a l 

   -- handle the combinator
   instance
      ( AlgVariantF (RemoveOddEven ys) (EADT ys) xs
      ) => RemoveOddEven ys (VariantF xs)
      where
         removeOE = algVariantF @(RemoveOddEven ys) removeOE

   -- handle remaining constructors
   instance {-# OVERLAPPABLE #-} f :<: ys => RemoveOddEven ys f where
      removeOE = VF -- keep the other constructors as is

We can test it:

.. code:: haskell

   eo2 :: EADT '[ConsF Int, EvenF Int, OddF Int, NilF]
   eo2 = Even (10 :: Int) $ Odd (5 :: Int) $ Cons (7 :: Int) $ Odd (7 :: Int) Nil

   > eadtShow (cata removeOE eo2 :: List Int)
   "10 : 5 : 7 : 7 : Nil"

   eo3 :: EADT '[ConsF Int, EvenF Int, OddF Int, ConsF String, NilF]
   eo3 = Even (10 :: Int) $ Cons "Test" $ Odd (5 :: Int) $ Cons (7 :: Int) $ Odd (7 :: Int) Nil

   > eadtShow (cata removeOE eo3 :: EADT '[ConsF Int, ConsF String, NilF])
   "10 : \"Test\" : 5 : 7 : 7 : Nil"


Note that we need to specify the resulting type as it could be anything
fulfilling the constraints.
