==============================================================================
Setting and matching values
==============================================================================

------------------------------------------------------------------------------
Setting values
------------------------------------------------------------------------------

The easiest way to set a variant value is to use the ``V`` pattern synonym:

.. code::

   x :: V '[String,Int]
   x = V "test"

The type of the value given to ``V`` must be statically determined as it is used
to find the index in the type list. For instance the following fails:

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

Indeed the type of ``10`` is ambiguous (``10 :: Num p => p``) and must be
explicitly disambiguated:

.. code::

   x :: V '[String,Int]
   x = V @Int 10

.. note::
   
   For now the compiler **cannot use the type list to infer the type of the
   value**!  In the previous example we have to specify the ``Int`` type even if
   it's clear (for us) that it's the obvious unique possibility.

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
