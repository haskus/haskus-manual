{-# LANGUAGE LambdaCase #-}

module EadtIntro where

data Expr n -- "n" represents a variable name
   = Lambda n (Expr n)
   | Var n
   | App (Expr n) (Expr n)

prettyPrint :: Show n => Expr n -> String
prettyPrint = \case
   Var n      -> show n
   Lambda n e -> mconcat ["\\",show n,".",prettyPrint e]
   App e1 e2  -> mconcat ["(",prettyPrint e1,") (",prettyPrint e2,")"]


sampleDouble :: Expr String
sampleDouble = Lambda "x" (Var "+" `App` Var "x" `App` Var "x")
