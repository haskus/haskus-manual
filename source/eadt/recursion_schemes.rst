.. _eadt_recursion_schemes:

==============================================================================
Recursion schemes
==============================================================================

Traversing an EADT explicitly (see :ref:`eadt_explicit_recursive`) can be
tedious. Another approach consists in using dedicated composable combinators
called **recursion schemes**.

.. note::

   ``map`` and ``fold`` are examples of recursion schemes for lists: these
   functions handle the recursive traversal of the data structure and are
   parameterized by the functions performing the actual work.
   Recursion schemes are a generalization of this approach.

The best introduction to recursion schemes I've read can be found here:
https://blog.sumtypeofway.com/an-introduction-to-recursion-schemes/

To avoid paraphrasing, I recommend that you read it before continuing.


Catamorphism: Show example
--------------------------

Suppose we rewrite our ``Show`` class like this:

.. code:: haskell

   class FunctorShow (f :: * -> *) where
      functorShow :: f String -> String

We can define instances for ``NilF`` and ``ConsF``:

.. code:: haskell

   instance FunctorShow NilF where
      functorShow _ = "Nil"

   instance (Show a) => FunctorShow (ConsF a) where
      functorShow (ConsF a l) = show a ++ " : " ++ l

Note that there is no recursive call in the definition of ``ConsF``'s instance:
it is because we are going to use a recursion scheme that will handle the
recursion.

We also need an instance to handle the generic ``VariantF`` type:

.. code:: haskell

   instance (AlgVariantF FunctorShow String xs) => FunctorShow (VariantF xs) where
      functorShow = algVariantF @FunctorShow functorShow

Finally we can define a generic ``eadtShow`` function that uses the catamorphism
recursion scheme with the ``functorShow`` class method.

.. code:: haskell

   eadtShow :: 
      ( Functor (VariantF xs)
      , FunctorShow (VariantF xs)
      ) => EADT xs -> String
   eadtShow = cata functorShow

We can test it:

.. code:: haskell

   intList :: List Int
   intList = Cons (10 :: Int) $ Cons (20 :: Int) $ Cons (30 :: Int) Nil

   mixedList :: EADT '[ConsF Int, ConsF Float, ConsF String, NilF]
   mixedList = Cons @Int 10 $ Cons @Float 5.0 $ Cons "Test" Nil

   > putStrLn $ eadtShow intList
   10 : 20 : 30 : Nil

   > putStrLn $ eadtShow mixedList
   10 : 5.0 : "Test" : Nil



Catamorphism: List (a -> a) mapping example
-------------------------------------------

Similarily to the example above, suppose that we want to implement mapping over
an EADT list. We can use the following type-class:

.. code:: haskell

   class MapEADT a xs (f :: * -> *) where
     -- map the outer constructor of an EADT
     mapEADT1 :: (a -> a) -> f (EADT xs) -> EADT xs

We need some instances to handle our EADT constructors:

.. code:: haskell

   instance (NilF :<: xs) => MapEADT a xs NilF where
     mapEADT1 _ NilF = Nil

   instance (ConsF a :<: xs) => MapEADT a xs (ConsF a) where
     mapEADT1 f (ConsF a x) = Cons (f a) x

And a additional instance to traverse the ``VariantF`` combinator datatype:

.. code:: haskell

   instance (AlgEADT (MapEADT a xs) xs) => MapEADT a xs (VariantF xs) where
     mapEADT1 f = algVariantF @(MapEADT a xs) (mapEADT1 f)

Now we can define the ``mapEADT`` function by using the catamorphism combinator:

.. code:: haskell

   -- recursively map an EADT
   mapEADT :: ( Functor (VariantF xs)
              , MapEADT a xs (VariantF xs)
              ) => (a -> a) -> EADT xs -> EADT xs
   mapEADT f = cata (mapEADT1 f)


We can test it:

.. code:: haskell

   intList :: List Int
   intList = Cons (10 :: Int) $ Cons (20 :: Int) $ Cons (30 :: Int) Nil

   > putStrLn $ eadtShow $ mapEADT ((+5) :: Int -> Int) intList
   15 : 25 : 35 : Nil


Catamorphism: List (a -> b) mapping example
-------------------------------------------

Similarily, we can also support mapping with a function that changes the EADT
type as follow:

.. code:: haskell

   class TransEADT a b xs xs' (f :: * -> *) where
     transEADT1 :: (a -> b) -> f (EADT xs) -> EADT xs'

   instance (NilF :<: xs') => TransEADT a b xs xs' NilF where
     transEADT1 _ NilF = Nil

   instance (ConsF b :<: xs', xs ~ xs') => TransEADT a b xs xs' (ConsF a) where
     transEADT1 f (ConsF a x) = Cons (f a) x

   instance TransEADT a b xs xs' (VariantF '[]) where
     transEADT1 _ _ = undefined

   instance
      ( TransEADT a b xs xs' f
      , TransEADT a b xs xs' (VariantF fs)
      ) => TransEADT a b xs xs' (VariantF (f ': fs)) where
     transEADT1 f v =  case popVariantFHead v of
            Right u -> transEADT1 f u
            Left  w -> transEADT1 f w

   transEADT :: ( Functor (VariantF xs)
                , TransEADT a b xs' xs' (VariantF xs)
                ) => (a -> b) -> EADT xs -> EADT xs'
   transEADT f = cata (transEADT1 f)


Note that we need to specify the resulting type as it can be anything fulfilling
the constraints:

.. code:: haskell

   > putStrLn $ eadtShow $ (transEADT (fromIntegral :: Int -> Float) intList :: List Float)
   10.0 : 20.0 : 30.0 : Nil

