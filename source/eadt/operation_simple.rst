.. _eadt_op_recursive_traversal:

==============================================================================
Recursive traversal
==============================================================================

To define a generic EADT recursive traversal, we use a type-class. For instance,
let's define a class ``MyShow`` that is very much like ``Show`` and that we will
use to print any EADT value:

.. code::

   class MyShow e where
      myShow :: e -> String

We can define instances for the ``List`` constructors defined in a
:ref:`previous chapter <eadt_basics>`:

.. code::

   instance MyShow (NilF e) where
      myShow _ = "Nil"

   instance (MyShow e, Show a) => MyShow (ConsF a e) where
      myShow (ConsF a l) = show a ++ " : " ++ myShow l

It also requires some additional boilerplate code (always quite the same for
each class) to work on any EADT:

.. code::

   {-# LANGUAGE UndecidableInstances #-}

   instance MyShow (f (Fix f)) => MyShow (Fix f) where
      {-# INLINE myShow #-}
      myShow (Fix e) = myShow e

   instance MyShow (VariantF '[] e) where
      {-# INLINE myShow #-}
      myShow = undefined

   instance
         ( MyShow (f e)
         , MyShow (VariantF fs e)
         ) => MyShow (VariantF (f ': fs) e)
      where
         {-# INLINE myShow #-}
         myShow v = case popVariantFHead v of
            Right u -> myShow u
            Left  w -> myShow w

Now we can test it:

.. code::

   strList :: List String
   strList = Cons "How" (Cons "are" (Cons "you?" Nil))

   intList :: List Int
   intList = Cons (10 :: Int) $ Cons (20 :: Int) $ Cons (30 :: Int) Nil

   mixedList :: EADT '[ConsF Int, ConsF Float, NilF]
   mixedList = Cons (10 :: Int) $ Cons (5.0 :: Float) $ Cons (30 :: Int) Nil

   > putStrLn (myShow strList)
   "How" : "are" : "you?" : Nil

   > putStrLn (myShow intList)
   10 : 20 : 30 : Nil

   > putStrLn (myShow mixedList)
   10 : 5.0 : 30 : Nil

------------------------------------------------------------------------------
Extensibility
------------------------------------------------------------------------------

If we add a new constructor, such as ``NodeF`` to build binary trees:

.. code::

   data NodeF a l = NodeF a l l deriving (Functor)

   pattern Node :: NodeF a :<: xs => a -> EADT xs -> EADT xs -> EADT xs
   pattern Node a l1 l2 = VF (NodeF a l1 l2)

We can also add a ``MyShow`` instance for ``NodeF``:

.. code::

   instance (MyShow e, Show a) => MyShow (NodeF a e) where
      myShow (NodeF a l1 l2) = show a ++ "\n|- " ++ indent (myShow l1)
                                      ++ "|- " ++ indent (myShow l2)
         where
            indent' []     = []
            indent' (x:xs) = x : fmap ("   "++) xs
            indent = unlines . indent' . lines

Now we can show trees as well as lists:

.. code::

   tree :: EADT '[NodeF Int, NilF]
   tree = Node (10 :: Int)
            (Node (5 :: Int) Nil Nil)
            (Node (30 :: Int) Nil Nil)
            

   > putStr (myShow tree)
   10
   |- 5
      |- Nil
      |- Nil
   |- 30
      |- Nil
      |- Nil

We can also mix up trees and lists by using ``ConsF`` and ``NodeF`` in the same
EADT:

.. code::

   mixedTree :: EADT '[NodeF Int, ConsF Int, NilF]
   mixedTree = Node (10 :: Int)
            (Cons (5 :: Int) $ Cons (6 :: Int) $ Cons (7 :: Int) Nil)
            (Node (30 :: Int) Nil Nil)

   > putStr (myShow mixedTree)
   10
   |- 5 : 6 : 7 : Nil
   |- 30
      |- Nil
      |- Nil


