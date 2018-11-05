.. _eadt_basics:

==============================================================================
Basics
==============================================================================

EADTs can be found in the `haskus-utils-variant
<https://github.com/haskus/haskus-utils>`_ package.

You need the following imports in your source:

.. code::

   import Haskus.Utils.EADT
   import Haskus.Utils.EADT.TH -- template-haskell helpers


------------------------------------------------------------------------------
Defining constructors
------------------------------------------------------------------------------

EADT constructors are data types that must have a ``Functor`` type-class instance.
Fortunately defining such data types is easy thanks to the ``DeriveFunctor``
extension that automatically generates the Functor instance for us.

For instance, let's define the constructors for a list:

.. code::

   {-# LANGUAGE DeriveFunctor #-}

   data ConsF a e = ConsF a e deriving (Functor)
   data NilF    e = NilF      deriving (Functor)

Note that **both** data types are parameterised (by ``e``) even if the type
parameter ``e`` isn't used in ``NilF`` definition.

------------------------------------------------------------------------------
Defining pattern synonyms
------------------------------------------------------------------------------

To make the use of EADTs more pleasant, it is highly recommended to define a
pattern synonym for each constructor:

.. code::

   pattern Cons :: ConsF a :<: xs => a -> EADT xs -> EADT xs
   pattern Cons a l = VF (ConsF a l)

   pattern Nil :: NilF :<: xs => EADT xs
   pattern Nil = VF NilF

These patterns hide the use of the ``VF`` pattern and make the code much easier
to work with.

As this code is very straightforward to write, we provide Template-Haskell
helpers to generate them automatically. The previous patterns can be generated
with:

.. code::

   {-# LANGUAGE TemplateHaskell #-}

   import Haskus.Utils.EADT.TH

   eadtPattern 'ConsF "Cons"
   eadtPattern 'NilF  "Nil"


------------------------------------------------------------------------------
Defining the EADT
------------------------------------------------------------------------------

An EADT is just a type alias as in the following ``List`` EADT example:

.. code::

   type List a = EADT '[ConsF a, NilF]

------------------------------------------------------------------------------
Creating values
------------------------------------------------------------------------------

Thanks to the pattern synonyms defined above, we can define values as we would
with a normal ADT:

.. code::

   strList :: List String
   strList = Cons "How" (Cons "are" (Cons "you?" Nil))

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


We could also easily define another pattern synonym when we work on ``List`` to
help the inference algorithm:

.. code::

   -- pattern for a specific EADT: List a
   pattern ConsList :: a -> List a -> List a
   pattern ConsList a l = Cons a l

We can see that when we use it we don't need type ascriptions because the
``Int`` type is propagated:

.. code ::

   intList :: List Int
   intList = ConsList 10 $ ConsList 20 $ ConsList 30 Nil


------------------------------------------------------------------------------
Matching values
------------------------------------------------------------------------------

It is easy and tempting to use the same pattern synonyms to match EADT values.
And indeed this works pretty well:

.. code::

   showEADTList :: Show a => List a -> String
   showEADTList = \case
      ConsList a l -> show a ++ " : " ++ showEADTList l
      Nil          -> "Nil"
      _            -> undefined

   > putStrLn (showEADTList strList)
   "How" : "are" : "you?" : Nil

   > putStrLn (showEADTList intList)
   10 : 20 : 30 : Nil


However this approach is a unsatisfactory for two reasons:

   1.  The pattern matching isn't safe: for now the compiler cannot use the
       EADT constructor type list to infer that the pattern-match is
       complete. Hence we need the wildcard match to avoid a warning and to
       use ``ConsList`` to help the type inference. A better alternative is
       presented in the :ref:`safe pattern-matching
       <eadt_safe_pattern_matching>` chapter.

   2. The function isn't generic: if we would like to write a ``showEADTList``
      function that also works on the heterogeneous ``mixedList`` above or on
      any future EADT provided its constructors can be handled, we need to
      use another approach based on type-classes. This is presented in the
      following chapters. 
