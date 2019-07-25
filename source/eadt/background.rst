.. _eadt_background:

==============================================================================
Background
==============================================================================

------------------------------------------------------------------------------
Why not Variant?
------------------------------------------------------------------------------

Extensible ADT (EADT) is a solution based on :ref:`Variant <variant>` that
supports recursive datatypes. Indeed if we try to define a recursive datatype
(e.g., a list) by using Variants, we get the following error:

.. code::

   data Cons a l = Cons a l
   data Nil      = Nil

   > type L a = V '[Cons a (L a), Nil]

   <interactive>:19:2: error:
       Cycle in type synonym declarations:
         <interactive>:19:2-34: type L a = V '[Cons a (L a), Nil]

We could introduce ad-hoc datatypes (e.g., ``newtype L a = L (V '[Cons a (L
a),Nil])``) but this would defeat our purpose because the datatype wouldn't be
generic anymore. Instead with EADTs we just have to write the following code to
declare a ``List`` EADT:

.. code::

   data ConsF a l = ConsF a l deriving (Functor)
   data NilF    l = NilF      deriving (Functor)

   type List a = EADT '[ConsF a, NilF]


------------------------------------------------------------------------------
History
------------------------------------------------------------------------------

The expression problem (1998)
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

In 1998, Philip Wadler defined the `Expression
<https://en.wikipedia.org/wiki/Expression_problem>`_ `Problem
<http://homepages.inf.ed.ac.uk/wadler/papers/expression/expression.txt>`_ as
follow:

   The Expression Problem is a new name for an old problem. The goal is
   to define a datatype by cases, where one can add new cases to the
   datatype and new functions over the datatype, without recompiling
   existing code, and while retaining static type safety

In Haskell it is straightforward to add new functions over an ADT. Suppose
we have the following arithmetic expression ADT:

.. code:: haskell

   data Expr = Val Int | Add Expr Expr

We can independently add an evaluator function, potentially in another module:

.. code:: haskell

   eval :: Expr -> Int
   eval (Val x)   =  x
   eval (Add x y) = eval x + eval y

However if we want to add a new constructor to the ADT (say support for
multiplication), we have to modify both the ADT definition and the functions
using it:

.. code:: haskell

   data Expr = .... | Mul Expr Expr

   eval :: Expr -> Int
   ....
   eval (Mul x y) = eval x * eval y

What we want is to be able to add a new independent module containing both the
``Mul`` constructor and the code to handle it, without modifying the other
modules defining the other constructors and the other code to handle them!

Data types à la carte (2008)
~~~~~~~~~~~~~~~~~~~~~~~~~~~~

Ten years later (in 2008), Wouter Swierstra described a technique to handle
this in his well-known `Data types à la carte
<http://www.cs.ru.nl/~W.Swierstra/Publications/DataTypesALaCarte.pdf>`_ paper.
The first idea is to define data constructors independently of the ADT and to
use a type parameter to leave open the ADT they are part of.

.. code:: haskell

   -- Independent data constructors. Parameter `e` represents the ADT they
   -- will be part of. It is required even if it is not used in the right hand
   -- side.
   data Val e = Val Int deriving (Functor)
   data Add e = Add e e deriving (Functor)

Defining a new independent constructor is easy:

.. code:: haskell

   data Mul e = Mul e e deriving (Functor)

The second idea is to use a combinator data type ``:+:``:

.. code:: haskell

   data (f :+: g) e = Inl (f e) | Inr (g e)

   instance (Functor f, Functor g) => Functor (f :+: g) where ...

It is similar to ``Either`` except that it passes the same additional type
parameter to both ``f`` and ``g`` type constructors. It can be used to compose
independent data constructors without creating a new data type:

.. code:: haskell

   type ExprF = Val :+: Add

``ExprF`` has kind ``Type -> Type`` and its type parameter is used as the ``e``
parameter of the independent data constructors. We can set it to arbitrary types
such as ``Int`` to build valid values:

.. code:: haskell

   y = Inr (Add 5 8) :: ExprF Int

However the main use of this parameter should be to indicate the type of the
expression data type we want to build, say ``Expr``. Hence we would like to
write something like this:

.. code:: haskell

   type Expr = ExprF Expr

    >error:
    Cycle in type synonym declarations:
      <interactive>:12:1-22: type Expr = ExprF Expr

Oops, we can't build this cyclic (infinite) type. This leads us to the third
idea: use another data type to handle the recursive nature of the expression
type:

.. code:: haskell

   newtype Expr = Expr (ExprF Expr)

We can abstract over it to use the same data type for different expression types:

.. code:: haskell

   -- `Fix` type as defined in Data.Functor.Foldable for instance
   newtype Fix f = Fix (f (Fix f))

   type Expr = Fix ExprF

In summary, the approach uses 3 different sorts of data types:

1. Constructor data types: ``Val``, ``Add``, ``Mul``...

2. Combinator data type: ``:+:``

3. Recursivity handling data type: ``Fix``

By using these different data types we have untangled the construction of ADTs
(algebraic data types) and we can freely add new constructor data types and mix
them into different algebraic data types.

Operations on these algebraic data types can be defined independently by using
type-classes and recursion schemes.

EADT - Extensible ADT (2018)
~~~~~~~~~~~~~~~~~~~~~~~~~~~~

The EADT approach builds on the Swierstra's one but it replaces the combinator
data type ``:+:`` with the ``VariantF`` one based on :ref:`Variant <variant>`.
Similarly to the ``:+:`` combinator data type, ``VariantF`` passes its ``e``
parameter to all of its "member" types and has an instance of the ``Functor``
class.

.. code:: haskell

   newtype VariantF (xs :: [* -> *]) e = VariantF (Variant (ApplyAll e xs))

   -- ApplyAll e '[f,g,h] ==> '[f e, g e, h e]

   instance Functor (VariantF xs) where ....


Now instead of writing ``f :+: g :+: h :+: i`` to combine constructor data types
to form an ADT we can write ``VariantF '[f,g,h,i]``.  Just like using
``Variant`` is more efficient -- O(1) memory usage and (de)construction -- than
using a nest of ``Either``, using ``VariantF`` is more efficient than using a
nest of ``:+:``.

Finally an EADT is just ``Fix (VariantF xs)`` except that we use our own
``EADT`` newtype instead of ``Fix`` in order to define our own additional (and
non-orphan) type-classes. ``EADT`` implements ``Recursive`` and ``CoRecursive``
type-classes so usual ``Fix`` functions should work on ``EADT`` too.

.. code:: haskell

   newtype EADT xs = EADT (VariantF xs)


With modern Haskell we can define :ref:`bidirectional pattern synonyms
<eadt_pattern_synonyms>` that make the manipulation of EADT values very similar
to the manipulation of usual ADTs.

In summary EADTs provide a nicer interface and a better asymptotic
implementation in both memory and runtime execution than Data types à la carte.
