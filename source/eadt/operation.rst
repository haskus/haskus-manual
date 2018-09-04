.. _eadt_generic:

==============================================================================
Generic operations
==============================================================================

To define generic EADT operations, we use type-classes. For instance, let's
implement ``Show`` for any EADT value:

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

   > myShow strList 
   "\"How\" : \"are\" : \"you?\" : Nil"

   > myShow intList
   "10 : 20 : 30 : Nil"

   > myShow mixedList 
   "10 : 5.0 : 30 : Nil"
