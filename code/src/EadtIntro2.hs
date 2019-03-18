{-# LANGUAGE LambdaCase #-}

module EadtIntro2 where

import EadtIntro

data AExpr a n -- "n" represents a variable name
               -- "a" represents an annotation
   = ALambda n (AExpr a n)
   | AVar n
   | AApp (AExpr a n) (AExpr a n)
   | Ann a (AExpr a n)

prettyPrintA :: (Show n, Show a) => AExpr a n -> String
prettyPrintA = \case
   AVar n      -> show n
   ALambda n e -> mconcat ["\\",show n,".",prettyPrintA e]
   AApp e1 e2  -> mconcat ["(",prettyPrintA e1,") (",prettyPrintA e2,")"]
   Ann a e     -> mconcat ["{",show a,"} ", prettyPrintA e]


sampleDoubleA :: AExpr a String
sampleDoubleA = ALambda "x" (AVar "+" `AApp` AVar "x" `AApp` AVar "x")

sampleAnnA :: AExpr String String
sampleAnnA = Ann "Double its input" sampleDoubleA

stripAnn :: AExpr a n -> Expr n
stripAnn = \case
   AVar n      -> Var n
   ALambda n e -> Lambda n (stripAnn e)
   AApp e1 e2  -> App (stripAnn e1) (stripAnn e2)
   Ann _a e    -> stripAnn e

toAnn :: Expr n -> AExpr a n
toAnn = \case
   Var n      -> AVar n
   Lambda n e -> ALambda n (toAnn e)
   App e1 e2  -> AApp (toAnn e1) (toAnn e2)
