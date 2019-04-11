{-# LANGUAGE DataKinds #-}

module Excepts where

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
