.. _variant_do_notation_either:

==============================================================================
Combining Either and Variant
==============================================================================

Suppose that we have some safe functions ``head``, ``lookup`` and ``parse`` that
return either a ``Variant`` (``Left``/error case) or a correct value (``Right``
case):

.. code:: haskell

   {-# LANGUAGE TypeApplications #-}

   import Haskus.Utils.Variant

   import Prelude hiding (head,lookup)
   import qualified Prelude
   import Text.Read

   data ParseError = ParseError deriving Show

   parse :: String -> Either (V '[ParseError]) Integer
   parse s = case readMaybe s of
      Just i  -> Right i
      Nothing -> Left (V ParseError)


   data HeadError = ListWasEmpty deriving Show

   head :: [a] -> Either (V '[HeadError]) a
   head []    = Left (V ListWasEmpty)
   head (x:_) = Right x

   data LookupError k = KeyWasNotPresent k deriving Show

   lookup :: Eq k => k -> [(k,v)] -> Either (V '[LookupError k]) v
   lookup k vs = case Prelude.lookup k vs of
      Just v  -> Right v
      Nothing -> Left (V (KeyWasNotPresent k))

To compose these functions, we can use ``liftVariant`` as follows:

.. code:: haskell

   import Data.Bifunctor (first)

   liftLeft :: LiftVariant xs ys => Either (V xs) r -> Either (V ys) r
   liftLeft = first liftVariant

   foo str = do
      c <- liftLeft $ head str
      r <- liftLeft $ lookup c codeMap
      liftLeft $ parse (r ++ tail str)

      where
         codeMap :: [(Char, String)]
         codeMap = [ ('x', "0x")
                   , ('d', "")
                   ]

1) We can fix the ``Variant`` type at the definition site:

.. code:: haskell

   foo :: String -> Either (V '[ParseError, LookupError Char, HeadError]) Integer

   -- The order of the error types doesn't matter and we can add additional error
   -- types if we want:
   foo :: String -> Either (V '[Float,Int,ParseError,LookupError Char,HeadError,String]) Integer

Test:

.. code:: haskell

   > foo "d10"
   Right 10

   > foo "x10"
   Right 16

   > foo "u10"
   Left V @(LookupError Char) (KeyWasNotPresent 'u')

   > foo ""
   Left V @HeadError ListWasEmpty

   > foo "d10X"
   Left V @ParseError ParseError


2) Or if don't give ``foo`` a type signature we can fix the ``Variant`` type when we call it:

.. code:: haskell

   > foo "d10" :: Either (V '[ParseError,HeadError,LookupError Char]) Integer
   Right 10

   -- The order of the error types still doesn't matter and we can add additional
   -- error types if we want:
   > foo "d10" :: Either (V '[Float,Int,ParseError,LookupError Char,HeadError,String]) Integer
   Right 10

3) Or we can give a generic type signature to ``foo``:

.. code:: haskell

   foo :: forall es.
      ('[HeadError,ParseError,LookupError Char] :<< es
      ) => String -> Either (V es) Integer

It allows us to use ``TypeApplications`` to pass the list of error types:

.. code:: haskell

   > foo @'[ParseError,HeadError,LookupError Char] "d10"
   Right 10

   > foo @'[HeadError,LookupError Char,ParseError] "d10X"
   Left V @ParseError ParseError

