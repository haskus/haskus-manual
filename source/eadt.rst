.. _eadt:

==============================================================================
Extensible ADT (EADT)
==============================================================================

EADTs (extensible algebraic data types) are ADTs whose constructors are defined
independently of the EADT. In the following example, we define two constructors
``ConsF`` and ``NilF`` and then we combine them to form a ``List`` EADT:

.. code::

   import Haskus.Utils.EADT

   data ConsF a l = ConsF a l deriving (Functor)
   data NilF    l = NilF      deriving (Functor)

   type List a = EADT '[ConsF a, NilF]

We use a type-level list to indicate the valid constructors for the EADT.
Compared to usual ADT (e.g. ``data List a = Cons a (List a) | Nil``) it allows
us to manipulate (extend/filter/etc.) the list of constructors in a type-safe
way and without requiring new data types. Moreover, the constructors can be
shared between several EADTs. For instance, the following code reuses ``NilF``
constructor to define a binary tree EADT:

.. code::

   data NodeF a l = NodeF a l l deriving (Functor)

   type Tree a = EADT '[NodeF a, NilF]

EADTs are based on the :ref:`Variant <variant>` sum type to which they add
support for recursive ADTs as in the examples above.

.. toctree::
   :maxdepth: 1
   :numbered:

   eadt/intro
   eadt/basics
   eadt/operation_simple
   eadt/operation_transformation
   eadt/recursion_schemes
   eadt/safe_pattern_matching
   eadt/constructor_removal
