.. _eadt_recursion_schemes:

==============================================================================
Recursion schemes
==============================================================================

Catamorphism: Show example
--------------------------

Suppose we rewrite our ``Show`` class like this:

.. code:: haskell

   class MyShow' (f :: * -> *) where
      myShow' :: f String -> String

We can define instances for ``NilF`` and ``ConsF``:

.. code:: haskell

   instance MyShow' NilF where
      myShow' _ = "Nil"

   instance (Show a) => MyShow' (ConsF a) where
      myShow' (ConsF a l) = show a ++ " : " ++ l

Note that there is no recursive call in the definition of ``ConsF``'s instance:
it is because we are going to use a recursion scheme that will handle the
recursion.

We also need an instance to handle the generic ``VariantF`` type:

.. code:: haskell

   instance (AlgVariantF MyShow' String xs) => MyShow' (VariantF xs) where
      myShow' = algVariantF @MyShow' @String @xs myShow'

Finally we can define a generic ``eadtShow`` function that uses the catamorphism
recursion scheme with the ``myShow'`` class method.

.. code:: haskell

   eadtShow :: 
      ( Functor (VariantF xs)
      , AlgVariantF MyShow' String xs
      ) => EADT xs -> String
   eadtShow = cata myShow'

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



Catamorphism: List mapping example
----------------------------------

Similarily to the example above, suppose that we want to implement mapping over
an EADT list. We can use the following type-class:

.. code:: haskell

   class MapList a a' r (f :: * -> *) where
     fmapList' :: (a -> a') -> f (EADT r) -> EADT r

We need some instances to handle our EADT constructors:

.. code:: haskell

   instance (NilF :<: r) => MapList a a' r NilF where
     fmapList' _ NilF = Nil

   instance (ConsF a' :<: r) => MapList a a' r (ConsF a) where
     fmapList' f (ConsF a x) = Cons (f a) x

And a boilerplate instance to traverse the generic ``VariantF``:

.. code:: haskell

   instance (AlgEADT (MapList a a' r) r) => MapList a a' r (VariantF r) where
     fmapList' f = algVariantF @(MapList a a' r) (fmapList' f)

Now we can define the ``fmapList`` function by using a recursion scheme named "catamorphism" (``cata`` for short):

.. code:: haskell

   fmapList :: (Functor (VariantF r) , MapList a a' r (VariantF r))
               => (a -> a') -> EADT r -> EADT r
   fmapList f = cata (fmapList' f)


We can test it:

.. code:: haskell

   intList :: List Int
   intList = Cons (10 :: Int) $ Cons (20 :: Int) $ Cons (30 :: Int) Nil

   > putStrLn $ myShow $ fmapList ((+5) :: Int -> Int) intList
   15 : 25 : 35 : Nil
