.. _variant_excepts:

==============================================================================
Excepts (ExceptT-like approach)
==============================================================================


Just like `ExceptT e m a
<https://www.stackage.org/haddock/lts-12.17/transformers-0.5.5.0/Control-Monad-Trans-Except.html#t:ExceptT>`_
wraps ``Either e m a``, we can use a ``Excepts es a`` newtype to wraps a
:ref:`VEither es a<veither>`.

Example:

.. code:: haskell

   import Haskus.Utils.Variant.Excepts

   import Prelude hiding (head,lookup)
   import qualified Prelude
   import Text.Read

   data ParseError = ParseError deriving Show

   parse :: String -> Excepts '[ParseError] IO Integer
   parse s = case readMaybe s of
      Just i  -> pure i
      Nothing -> throwE ParseError


   data HeadError = ListWasEmpty deriving Show

   head :: [a] -> Excepts '[HeadError] IO a
   head []    = throwE ListWasEmpty
   head (x:_) = pure x

   data LookupError k = KeyWasNotPresent k deriving Show

   lookup :: Eq k => k -> [(k,v)] -> Excepts '[LookupError k] IO v
   lookup k vs = case Prelude.lookup k vs of
      Just v  -> pure v
      Nothing -> throwE (KeyWasNotPresent k)


   foo :: String -> Excepts '[ParseError, LookupError Char, HeadError] IO Integer
   -- foo :: forall es.
   --    ('[HeadError,ParseError,LookupError Char] :<< es
   --    ) => String -> Excepts es IO Integer
   foo str = do
      c <- liftE $ head str
      r <- liftE $ lookup c codeMap
      liftE $ parse (r ++ tail str)

      where
         codeMap :: [(Char, String)]
         codeMap = [ ('x', "0x")
                   , ('d', "")
                   ]

Test:

.. code:: haskell

   > runE (foo "d10")
   VRight 10

   > runE (foo "x10")
   VRight 16

   > runE (foo "u10")
   VLeft KeyWasNotPresent 'u'

   > runE (foo "")
   VLeft ListWasEmpty

   > runE (foo "d10X")
   VLeft ParseError

   > runE (foo "" `catchE` (\ListWasEmpty -> success 42) :: Excepts '[ParseError,LookupError Char] IO Integer)
   VRight 42
