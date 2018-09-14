==============================================================================
Transformation (using ``:<:``)
==============================================================================

Suppose we have the following EADT for arithmetic expressions:

.. code::

   {-# LANGUAGE DeriveFunctor #-}

   data ValF e = ValF Int deriving (Functor)
   data AddF e = AddF e e deriving (Functor)
   data MulF e = MulF e e deriving (Functor)

   pattern Val :: ValF :<: xs => Int -> EADT xs
   pattern Val a = VF (ValF a)

   pattern Add :: AddF :<: xs => EADT xs -> EADT xs -> EADT xs
   pattern Add a b = VF (AddF a b)

   pattern Mul :: MulF :<: xs => EADT xs -> EADT xs -> EADT xs
   pattern Mul a b = VF (MulF a b)

   type Expr = EADT '[ValF, AddF, MulF]

We can define some value:

.. code::

   e1 :: Expr
   e1 = Add (Val 10)
            (Mul (Add (Val 5)
                      (Val 10))
                 (Val 7))


We can define instances of the ``MyShow`` class (defined :ref:`here
<eadt_op_recursive_traversal>`):

.. code::

   instance MyShow (ValF e) where
      myShow (ValF e) = show e

   instance MyShow e => MyShow (AddF e) where
      myShow (AddF x y) = "(" ++ myShow x ++ " + " ++ myShow y ++ ")"

   instance MyShow e => MyShow (MulF e) where
      myShow (MulF x y) = "(" ++ myShow x ++ " * " ++ myShow y ++ ")"

   > putStrLn (myShow e1)
   (10 + ((5 + 10) * 7))


Now we can define a transformation that distributes multiplication over
addition as follows:

.. code::

   -- distribute multiplication over addition if it matches
   distr :: (AddF :<: f, MulF :<: f) => EADT f -> Maybe (EADT f)
   distr (Mul a (Add c d)) = Just (Add (Mul a c) (Mul a d))
   distr (Mul (Add c d) a) = Just (Add (Mul c a) (Mul d a))
   distr _                 = Nothing

Note that this function works on any EADT as long as it has ``AddF`` and
``MulF`` constructors. We indicate such constraints with the ``:<:`` type
operator.

Then we need a helper function that performs the traversal of the EADT:

.. code::

   import Control.Arrow

   -- bottom up traversal that performs an additional bottom up traversal in
   -- the transformed sub-tree when a transformation occurs. 
   bottomUpFixed :: Functor (VariantF cs) => (EADT cs -> Maybe (EADT cs)) -> EADT cs -> EADT cs
   bottomUpFixed f = unfix >>> fmap (bottomUpFixed f) >>> Fix >>> f'
      where
         f' u = case f u of
            Nothing -> u
            Just v  -> bottomUpFixed f v

Finally we can test the transformation on an example:

.. code::

   > putStrLn (myShow e1)
   (10 + ((5 + 10) * 7))

   > putStrLn (myShow (bottomUpFixed distr e1))
   (10 + ((5 * 7) + (10 * 7)))

------------------------------------------------------------------------------
Extensibility
------------------------------------------------------------------------------

Suppose we add a ``Pow`` (power) constructor:

.. code::

   data PowF e = PowF e e deriving (Functor)

   pattern Pow :: PowF :<: xs => EADT xs -> EADT xs -> EADT xs
   pattern Pow a b = VF (PowF a b)

   instance MyShow e => MyShow (PowF e) where
      myShow (PowF x y) = "(" ++ myShow x ++ " ^ " ++ myShow y ++ ")"

We can now write expressions that use the ``Pow`` constructor:

.. code::

   type Expr2 = EADT '[ValF, AddF, MulF, PowF]

   e2 :: Expr2
   e2 = Pow (Val 10)
            (Mul (Add (Pow (Val 5) (Val 8))
                      (Val 10))
                 (Val 7))

We can check that our distribution function still works on this new type of
expression without being modified at all:

.. code::

   > putStrLn (myShow (bottomUpFixed distr e2))
   (10 ^ (((5 ^ 8) * 7) + (10 * 7)))


