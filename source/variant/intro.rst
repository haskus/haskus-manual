==============================================================================
Introduction
==============================================================================

------------------------------------------------------------------------------
Why do we need Variant?
------------------------------------------------------------------------------

In the functional programming world we use algebraic data types (ADT), more
specifically `sum types <https://en.wikipedia.org/wiki/Tagged_union>`_, to
indicate that a value can be of two or more different types:

.. code::

   x,y :: Either String Int
   x = Left "yo"
   y = Right 10

What if we want to support more than two types?

Solution 1: sum types
~~~~~~~~~~~~~~~~~~~~~

We could use different sum types with different constructors for each arity
(number of different types that the value can have).

.. code::
   
   data SumOf3 a b c   = S3_0 a | S3_1 b | S3_2 c
   data SumOf4 a b c d = S4_0 a | S4_1 b | S4_2 c | S4_3 d

But it's quite hard to work with that many different types and constructors as
we can't easily define generic functions working on different sum types without
a combinatorial explosion.

Solution 2: recursive ADT
~~~~~~~~~~~~~~~~~~~~~~~~~

Instead of adding new sum types we can use a nest of ``Either``:

.. code::

   type SumOf3 a b c   = Either a (Either b c)
   type SumOf4 a b c d = Either a (Either b (Either c d))


Or more generically:

.. code::

   data Union (as :: [*]) where
     Union :: Either (Union as) a -> Union (a ': as)

This time we can define generic functions without risking a combinatorial
explosion. The drawback however is that we have changed the representation:
instead of ``tag + value`` where ``tag`` is in the range [0,arity-1] we have a
nest of ``tag + (tag + (... (tag + value)))`` where ``tag`` is in the range
[0,1]. It is both inefficient in space and in time (accessing the tag value is
in O(arity)).

Solution 3: variant
~~~~~~~~~~~~~~~~~~~

``Variant`` gets the best of both approaches: it has the generic interface of
the "recursive ADT" solution and the efficient representation of the "sum types"
solution.

.. code::

   data Variant (types :: [*]) = Variant {-# UNPACK #-} !Word Any

   type role Variant representational

The efficient representation is ensured by the definition of the ``Variant``
datatype: an unpacked ``Word`` for the tag and a "pointer" to the value.

The phantom type list ``types`` contains the list of possible types for the value.
The tag value is used as an index in this list to know the effective type of the
value.

------------------------------------------------------------------------------
Using Variant
------------------------------------------------------------------------------

To use ``Variant``:

   * add a dependency to the `haskus-utils-variant <https://hackage.haskell.org/package/haskus-utils-variant>`_ package
   * use the following import: ``import Haskus.Utils.Variant``

You may need to enable some language extensions:

.. code::

   {-# LANGUAGE DataKinds #-}
   {-# LANGUAGE TypeOperators #-}
   {-# LANGUAGE FlexibleContexts #-}
   {-# LANGUAGE FlexibleInstances #-}
   {-# LANGUAGE TypeApplications #-}
   {-# LANGUAGE LambdaCase #-}
   {-# LANGUAGE TypeFamilies #-}
