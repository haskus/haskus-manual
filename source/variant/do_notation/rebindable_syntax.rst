==============================================================================
Rebindable sytnax
==============================================================================

We can use ``do-notation`` with ``Variant`` as we would with other sum types
such as ``Maybe`` or ``Either``. However, as we can't have a ``Monad`` instance
for ``Variant``, we rely on the ``RebindableSyntax`` extension to mimic it.

The leftmost type is extracted from the Variant with ``>>=`` (or ``x <-
myVariant`` with do-notation syntax). Variant types are concatenated on the
left.

Function ``foo`` in the following example composes functions returning Variants
by using do-notation:

.. code:: haskell

   {-# LANGUAGE TypeApplications #-}
   {-# LANGUAGE RebindableSyntax #-}

   import Haskus.Utils.Variant
   import Haskus.Utils.Variant.Syntax

   import Prelude hiding (head,lookup,(>>=),(>>),return)
   import qualified Prelude
   import Text.Read

   foo :: String -> V '[Integer, ParseError, LookupError Char, HeadError]
   foo str = do
      c <- head str
      r <- lookup c codeMap
      parse (r ++ tail str)

      where
         codeMap :: [(Char, String)]
         codeMap = [ ('x', "0x")
                   , ('d', "")
                   ]


   data ParseError = ParseError deriving Show

   parse :: String -> V '[Integer,ParseError]
   parse s = case readMaybe s of
      Just i  -> V @Integer i                -- we use the `V` pattern to index
      Nothing -> V ParseError                -- the Variant by type

   data HeadError = ListWasEmpty deriving Show

   head :: [a] -> V '[a,HeadError]
   head []    = toVariantAt @1 ListWasEmpty  -- we can't index the Variant by
   head (x:_) = toVariantAt @0 x             -- type because `a` is ambiguous,
                                             -- so we do it by index explicitly

   data LookupError k = KeyWasNotPresent k deriving Show

   lookup :: Eq k => k -> [(k,v)] -> V '[v,LookupError k]
   lookup k vs = case Prelude.lookup k vs of
      Just v  -> toVariantAt @0 v            -- ditto
      Nothing -> toVariantAt @1 (KeyWasNotPresent k)


Test:

.. code:: haskell

   > foo "d10"
   V @Integer 10

   > foo "x10"
   V @Integer 16

   > foo "u10"
   V @(LookupError Char) (KeyWasNotPresent 'u')

   > foo ""
   V @HeadError ListWasEmpty

   > foo "d10X"
   V @ParseError ParseError
