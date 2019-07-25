.. _eadt_explicit_recursive:

==============================================================================
Explicit recursive traversal
==============================================================================

When we need to traverse a data structure, we can either use predefined
traversal functions (e.g., ``map``, ``fold``, etc.) or write the recursive
function explicitly. EADTs are no different in this regard.

In this chapter we explain how to write explicitly recursive functions for
EADTs: similarly to usual ADTs, it's better to use them only when generic
traversal functions (presented in following chapters) don't fit the bill.

------------------------------------------------------------------------------
Traversal example
------------------------------------------------------------------------------

If we were to write a ``show`` function for a list ADT, we could do it like
this:

.. code::

   data List a = Cons a (List a) | Nil

   showList :: Show a => List a -> String
   showList = \case
      Nil      -> "Nil"
      Cons a l -> show a ++ " : " ++ showList l

In ``showList`` we can pattern match on the constructors of ``List a`` because
the constructor list is closed.  With EADTs the list of constructors isn't
closed and we want to be able to use the same code even with EADTs extended with
more constructors. To support this, we use type-classes to build the equivalent
of the ``case`` in ``showList`` above.


Let's define a class ``MyShow`` that is very much like ``Show`` and that we will
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

Note how each instance corresponds to an alternative in ``showList``.


It also requires some additional instances to traverse the ``VariantF``
combinator datatype and the ``EADT`` recursivity handling datatype:

.. code:: haskell

   {-# LANGUAGE UndecidableInstances #-}

   instance MyShow (f (EADT f)) => MyShow (EADT f) where
      {-# INLINE myShow #-}
      myShow (EADT e) = myShow e

   instance MyShow (VariantF [] e) where
      {-# INLINE myShow #-}
      myShow = undefined

   instance
         ( MyShow (f e)
         , MyShow (VariantF fs e)
         ) => MyShow (VariantF (f : fs) e)
      where
         {-# INLINE myShow #-}
         myShow v = case popVariantFHead v of
            Right u -> myShow u
            Left  w -> myShow w

.. note ::

   This boilerplate code (hopefully always very similar and straightforward) is the
   main reason you should strive to use predefined recursion schemes instead of the
   explicit approach presented here.

.. note::

   The INLINE pragmas are used to ensure that in the generated code we get the
   equivalent of the ``case`` expression in ``showList``.

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
Extension example
------------------------------------------------------------------------------

If we add a new constructor, such as ``NodeF`` to build binary trees:

.. code::

   data NodeF a e = NodeF a e e deriving (Functor)

   eadtPattern 'NodeF "Node"

We can also add a ``MyShow`` instance for ``NodeF``:

.. code::

   instance (MyShow e, Show a) => MyShow (NodeF a e) where
      myShow (NodeF a l1 l2) = show a ++ "\n|- " ++ indent (myShow l1)
                                      ++ "|- " ++ indent (myShow l2)
         where
            indent' []     = []
            indent' (x:xs) = x : fmap ("   "++) xs
            indent = unlines . indent' . lines

Now we can show binary trees as well as lists:

.. code::

   tree :: EADT '[NodeF Int, NilF]
   tree = Node (10 :: Int)
            (Node (5 :: Int) Nil Nil)
            (Node (30 :: Int) Nil Nil)
            

   > putStrLn (myShow tree)
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

   > putStrLn (myShow mixedTree)
   10
   |- 5 : 6 : 7 : Nil
   |- 30
      |- Nil
      |- Nil

   -- Note: the code to display trees isn't very clever so don't use it to
   -- display list of trees.
