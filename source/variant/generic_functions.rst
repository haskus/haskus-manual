==============================================================================
Writing generic functions using variants
==============================================================================

In this chapter we show how to write generic functions that can work on
different ``Variant`` as long as they fulfill some constraints.

------------------------------------------------------------------------------
The ``:<`` constraint constructor
------------------------------------------------------------------------------

The ``c :< cs`` constraint statically ensures that the type ``c`` is in the
``cs`` type list and that we can set and match it in a variant with type ``V
cs``. For example:

.. code::

   newtype Error = Error String

   showError :: (Error :< cs) => V cs -> String
   showError = \case
      V (Error s) -> "Found error: " ++ s
      _           -> "Not an Error!"

We check that ``showError`` works:
  
.. code::

   e0,e1 :: V '[String,Int,Error]
   e0 = V (Error "invalid")
   e1 = V @Int 10

   -- > showError e0
   -- "Found error: invalid"
   -- > showError e1
   -- "Not an Error!"

The same generic ``showError`` function works with variants of other types as
well:
  
.. code::

   e2 :: V '[Float,String,Maybe Char,Error]
   e2 = V (Error "Oups!")

   e3 :: V '[Error]
   e3 = V (Error "Outch!")

   -- > showError e2
   -- "Found error: Oups!"
   -- > showError e3
   -- "Found error: Outch!"


------------------------------------------------------------------------------
The ``:<?`` constraint constructor and the ``VMaybe`` pattern
------------------------------------------------------------------------------

The ``c :< cs`` constraint statically ensures that the type ``c`` is in the
``cs`` type list. However in some cases we want to write generic functions that
work on variants even if they can't contain the given type.

For instance if we try to apply the ``showError`` function of the previous
example on a variant that can't contain a value of type ``Error``, we get the
following expected compile-time error:

.. code::

   e4 :: V '[String,Int]
   e4 = V "valid"

   -- > showError e4
   -- 
   -- <interactive>:45:1: error:
   --     • `Error' is not a member of '[String, Int]


Nevertheless we can write a ``showErrorMaybe`` that works on any variant even if
it can't contain an ``Error`` value by using the ``:<?`` constraint constructor
and by matching with ``VMaybe`` as follows:

.. code::

   showErrorMaybe :: (Error :<? cs) => V cs -> String
   showErrorMaybe = \case
      VMaybe (Error s) -> "Found error: " ++ s
      _                -> "Not an Error!"

   -- > showErrorMaybe e0
   -- "Found error: invalid"
   -- > showErrorMaybe e1
   -- "Not an Error!"
   -- > showErrorMaybe e2
   -- "Found error: Oups!"
   -- > showErrorMaybe e3
   -- "Found error: Outch!"
   -- > showErrorMaybe e4
   -- "Not an Error!"

Obviously this example is a bit contrived because we can easily see that ``e4``
can't contain an ``Error``. However the same ``:<?`` constraint is also used to
define some more interesting operations as shown below.

------------------------------------------------------------------------------
Shrinking variants: ``popVariant``
------------------------------------------------------------------------------

A very common use of variants is to pattern match on a specific value type they
can contain and to get a new variant containing the left-over value types. This
is done with ``popVariant`` or ``popVariantMaybe`` and the ``Filter`` type
family. For example:

.. code::

   filterError :: Error :<? cs => V cs -> V (Filter Error cs)
   filterError v = case popVariantMaybe v of
      Right (Error s) -> error ("Found error: " ++ s)
      Left  v'        -> v' -- left-over variant!


   -- > filterError e0
   -- *** Exception: Found error: invalid
   -- CallStack (from HasCallStack):
   --   error, called at Test.hs:61:23 in main:Main

   -- > filterError e1
   -- 10

   -- > :t e1
   -- e1 :: V '[String, Int, Error]

   -- > :t filterError e1
   -- filterError e1 :: V '[String, Int]

   -- > :t e2
   -- e2 :: V '[Float, String, Maybe Char, Error]

   -- > :t filterError e2
   -- filterError e2 :: V '[Float, [Char], Maybe Char]

Notice how an ``Error`` value can't be present anymore in the variant type
returned by ``filterError`` and how this function is generic as it supports any
variant as an input.

Similarly we could have used the ``Error <: cs`` constraint and the
``popVariant`` function to ensure that only variants that can contain an
``Error`` value can be passed to the ``filterError`` function.
