==============================================================================
Setting and matching values
==============================================================================

------------------------------------------------------------------------------
Setting values
------------------------------------------------------------------------------

The easiest way to set a variant value is to use the ``V`` pattern synonym:

.. code::

   x,y :: V '[String,Int]
   x = V "test"
   y = V @Int 10

.. note::
   
   For now the compiler **cannot use the type list to infer the type of the
   value**!  In the previous example we have to specify the ``Int`` type. Even if
   it's clear (for us) that it's the obvious unique possibility, it is ambiguous
   for the compiler.

.. code::

   x :: V '[String,Int]
   x = V 10

   -- Test.hs:12:5: error:
   --     • Couldn't match type ‘Haskus.Utils.Types.List.IsMember'
   --                              '[String, Int] c0 '[String, Int]’
   --                      with ‘'True’
   --         arising from a use of ‘V’
   --       The type variable ‘c0’ is ambiguous
   --     • In the expression: V 10
   --       In an equation for ‘x’: x = V 10

------------------------------------------------------------------------------
Matching values
------------------------------------------------------------------------------

Matching a value in a variant can be done with the same pattern synonym ``V``:

.. code::

   f :: V '[String,Int] -> String
   f = \case
      V s            -> "Found string: " ++ s
      V (i :: Int)   -> "Found int: " ++ show i
      _              -> undefined

.. note::
   
   For now the compiler **cannot use the type list to infer that the
   pattern-match is complete**. Hence we need the wildcard match to avoid a
   warning.

------------------------------------------------------------------------------
Basic errors
------------------------------------------------------------------------------

If you try to set or match a value type that isn't valid, you get a compile-time error:

.. code::

   x :: V '[String,Int]
   x = V @Float 10

   -- Test.hs:14:5: error:
   --     • `Float' is not a member of '[String, Int]
   --     • In the expression: V @Float 10
   --       In an equation for ‘x’: x = V @Float 10


.. code::

   f :: V '[String,Int] -> String
   f = \case
      V s            -> "Found string: " ++ s
      V (i :: Int)   -> "Found int: " ++ show i
      V (i :: Float) -> "Found float: " ++ show i
      _              -> undefined

   -- Test.hs:20:4: error:
   --     • `Float' is not a member of '[String, Int]
   --     • In the pattern: V (i :: Float)
   --       In a case alternative: V (i :: Float) -> "Found float: " ++ show i

