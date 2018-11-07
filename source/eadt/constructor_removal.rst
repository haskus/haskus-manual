.. _eadt_constructor_removal:

==============================================================================
Constructor removal/transformation
==============================================================================

Removing constructors from an EADT is equivalent to transforming every instance
of these constructors into other constructors of another EADT.

We consider 3 cases:

    1. Fixed input EADT type; fixed list of constructors to act on

    2. Generic input EADT type; fixed list of constructors to act on

    3. Generic input EADT type; extensible list of constructors to act on

Note in the 3 cases we need to specify the resulting EADT type as it could be
anything fulfilling the constraints.

------------------------------------------------------------------------------
Fixed input, fixed matches
------------------------------------------------------------------------------

If the type of the input EADT is fixed, we can use :ref:`safe pattern-matching
<eadt_safe_pattern_matching>` as follows:

.. code::

   -- replace Even and Odd constructors with a Cons constructor
   removeOddEven l = toCont l >::>
      (\(EvenF a r) -> Cons a r
      ,\(OddF  a r) -> Cons a r
      ,\NilF        -> Nil
      )

   eo :: EADT '[EvenF Int, OddF Int, NilF]
   eo = Even (10 :: Int) $ Odd (5 :: Int) $ Odd (7 :: Int) Nil

   > eadtShow (cata removeOddEven eo :: List Int)
   "10 : 5 : 7 : Nil"

Note that ``removeOddEven`` only works on a specific EADT. If we want it to work on
any EADT that contains ``Even`` and ``Odd`` constructors, read the following
sections.

------------------------------------------------------------------------------
Generic input, fixed matches
------------------------------------------------------------------------------

If we want ``removeEvenOdd`` to work on input EADTs of any type, we can extract
the constructors that we are interested in with ``splitVariantF`` and lift the
left-over constructors with ``liftVariantF`` as follows:

.. code:: haskell

   removeOddEven x = case splitVariantF @'[EvenF Int, OddF Int] x of
      -- replace Even and Odd constructors with a Cons constructor
      Right v        -> toCont v >::>
                           ( \(EvenF a l) -> Cons a l
                           , \(OddF a l)  -> Cons a l
                           )
      -- do nothing to the other constructors
      Left leftovers -> Fix (liftVariantF leftovers)

   eo1 :: EADT '[EvenF Int, OddF Int, NilF]
   eo1 = Even (10 :: Int) $ Odd (5 :: Int) $ Odd (7 :: Int) Nil

   > eadtShow (cata removeOddEven eo1 :: List Int)
   "10 : 5 : 7 : Nil"

   -- additional `ConsF Int` constructor
   eo2 :: EADT '[ConsF Int, EvenF Int, OddF Int, NilF]
   eo2 = Even (10 :: Int) $ Cons (5 :: Int) $ Odd (7 :: Int) Nil

   > eadtShow (cata removeOddEven eo2 :: List Int)
   "10 : 5 : 7 : Nil"

------------------------------------------------------------------------------
Generic input, extensible matches
------------------------------------------------------------------------------

If we want the ``removeOddEven`` pattern match to be extensible, we can use
type-classes with an overlappable instance handling the generic case (i.e. that
only transfers constructors from one EADT to another without modifying them).

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

Test:

.. code:: haskell

   eo :: EADT '[EvenF Int, OddF Int, NilF]
   eo = Even (10 :: Int) $ Odd (5 :: Int) $ Odd (7 :: Int) Nil

   > eadtShow (cata removeOddEven eo :: List Int)
   "10 : 5 : 7 : Nil"

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

We can extend ``removeOddEven`` to support other constructors by adding new
instances of ``RemoveOddEven`` for them.

