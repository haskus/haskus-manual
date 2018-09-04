==============================================================================
Defining an EADT
==============================================================================

------------------------------------------------------------------------------
Defining constructors
------------------------------------------------------------------------------

EADT constructors are data types that must have a ``Functor`` type-class instance.
Fortunately defining such data types is easy thanks to the ``DeriveFunctor``
extension that automatically generates the Functor instance for us.

For instance, let's define the constructors for a list:

.. code::

   {-# LANGUAGE DeriveFunctor #-}

   newtype ConsF a e = ConsF a e deriving (Functor)
   newtype NilF    e = NilF      deriving (Functor)

Note that **both** data types are parameterised (by ``e``) even if the type
parameter ``e`` isn't used in ``NilF`` definition.

To make the use of EADTs more pleasant, it is highly recommended to define a
pattern synonym for each constructor as follows:

.. code::

   pattern Cons :: ConsF a :<: xs => a -> EADT xs -> EADT xs
   pattern Cons a l = VF (ConsF a l)

   pattern Nil :: NilF :<: xs => EADT xs
   pattern Nil = VF NilF

These patterns hide the use of the ``VF`` pattern and make the code much easier
to work with.

------------------------------------------------------------------------------
Defining the EADT
------------------------------------------------------------------------------

An EADT is just a type alias as in the following ``List`` EADT example:

.. code::

   type List a = EADT '[ConsF a, NilF]

------------------------------------------------------------------------------
Defining values
------------------------------------------------------------------------------

Thanks to the pattern synonyms defined above, we can define values as we would
with a normal ADT:

.. code::

   let xs = Cons "How" (Cons "are" (Cons "you?" Nil)) :: List String

In some cases we have to help the type-checker to determine some types. For
instance, in the following example it can't infer the ``a`` type in ``ConsF a``,
hence we have to use type ascriptions:

.. code::

   intList :: List Int
   intList = Cons (10 :: Int) $ Cons (20 :: Int) $ Cons (30 :: Int) Nil

This is because the code is generic enough that the same pattern synonyms could
be used to build an heterogeneous list. For instance containing both ``Int`` and
``Float``:

.. code::

   mixedList :: EADT '[ConsF Int, ConsF Float, NilF]
   mixedList = Cons (10 :: Int) $ Cons (5.0 :: Float) $ Cons (30 :: Int) Nil


We could easily define another pattern synonym when we work on ``List`` to help
the inference algorithm:

.. code::

   -- pattern for a specific EADT: List a
   pattern ConsList :: a -> List a -> List a
   pattern ConsList a l = Cons a l

   intList :: List Int
   intList = ConsList 10 $ ConsList 20 $ ConsList 30 Nil

