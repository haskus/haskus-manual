.. _eadt_safe_pattern_matching:

==============================================================================
Safe pattern matching with >:>
==============================================================================

Suppose we have the following ``List`` EADT:

.. code:: haskell

   data ConsF a l = ConsF a l deriving (Functor)
   data NilF    l = NilF      deriving (Functor)

   eadtPattern 'ConsF "Cons"
   eadtPattern 'NilF  "Nil"

   type List a = EADT '[ConsF a, NilF]

   -- pattern for a specific EADT: List a
   pattern ConsList :: a -> List a -> List a
   pattern ConsList a l = Cons a l


Using classic pattern matching on ``List`` constructors as we do below isn't
really typesafe because the compiler cannot detect that the pattern matching is
complete, hence we have the choice between a warning or adding a wildcard match:

.. code:: haskell

   showEADTList :: Show a => List a -> String
   showEADTList = \case
      ConsList a l -> show a ++ " : " ++ showEADTList l
      Nil          -> "Nil"
      _            -> undefined -- this line avoids the warning but is unsafe
                                -- if we add constructors in the future


A safe alternative is to rely on multi-continuations: we can transform any
``EADT '[A,B,C]`` into a function whose type is ``(A -> r, B -> r, C -> r) ->
r`` with the ``(>:>)`` operator. Then we can safely provide a function per
constructor as in a pattern-matching. 


**Explicit recursion example**

.. code:: haskell

   import Haskus.Utils.ContFlow

   showCont' l = l >:>
      ( \(ConsF a r) -> show a ++ " : " ++ showCont' r -- explicit recursion
      , \NilF        -> "Nil"
      )

   > showCont' intList
   "10 : 20 : 30 : Nil"

**Recursion schemes (e.g. catamorphism)**

.. code:: haskell

   showCont l = l >:>
      ( \(ConsF a r) -> show a ++ " : " ++ r -- no explicit recursion
      , \NilF        -> "Nil"
      )

   > cata showCont intList
   "10 : 20 : 30 : Nil"

See :ref:`recursion schemes <eadt_recursion_schemes>`.
