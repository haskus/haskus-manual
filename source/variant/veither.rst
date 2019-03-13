.. _variant_veither:

==============================================================================
VEither (biased variant)
==============================================================================

Variants have the following kind of types: ``V '[W,X,Y,Z]``. This is great when
all the inner types play the same role. However in some cases we want one type
to be the main one and the other ones to be secondaries.

For instance we could have the variant whose type is ``V
'[Result,ErrorA,ErrorB,ErrorC]`` to represent the result of a function. In this
case, the first type is the main one and it would be great to be able to define the
common type-classes (Functor, Monad, etc.) so that we have easy access to it.

``VEither`` is a Variant wrapper that does exactly this:

.. code:: haskell

   newtype VEither es a = VEither (V (a ': es))

It is isomorphic to the following type: ``Either (V es) a``. The difference is
in the runtime representation: ``VEither es a`` has one less indirection than
``Either (V es) a`` (it uses only one tag value).


------------------------------------------------------------------------------
VRight and VLeft
------------------------------------------------------------------------------

``VEither es a`` values can be created and matched on with the ``VRight`` and
``VLeft`` patterns (just as if we had the ``Either (V es) a`` type).

.. code:: haskell

   >>> VRight True :: VEither '[String,Int] Bool
   VRight True

   >>> VLeft (V "failed" :: V '[String,Int]) :: VEither '[String,Int] Bool
   VLeft (V @[Char] "failed" :: V '[[Char], Int])

------------------------------------------------------------------------------
Common instances
------------------------------------------------------------------------------

The main advantage of ``VEither es a`` over ``V (a ': es)`` is that we can
define instances for common type-classes such as Functor, Applicative, Monad,
Foldable, etc.:

.. code:: haskell

   >>> let x = VRight True :: VEither '[Int,Float] Bool
   >>> fmap (\b -> if b then "Success" else "Failure") x
   VRight "Success"

   >>> let x = VRight True  :: VEither '[Int,Float] Bool
   >>> let y = VRight False :: VEither '[Int,Float] Bool
   >>> (&&) <$> x <*> y
   VRight False

   >>> let x   = VRight True    :: VEither '[Int,Float] Bool
   >>> let f v = VRight (not v) :: VEither '[Int,Float] Bool
   >>> x >>= f
   VRight False

   >>> let x   = VRight True    :: VEither '[Int,Float] Bool
   >>> let y   = VLeft (V "failed" :: V '[String,Int]) :: VEither '[String,Int] Bool
   >>> forM_ x print
   True
   >>> forM_ y print
