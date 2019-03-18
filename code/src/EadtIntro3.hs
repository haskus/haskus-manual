{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE FlexibleContexts #-}

module EadtIntro3 where

import Haskus.Utils.EADT
import Haskus.Utils.EADT.TH

data LambdaF n e = LambdaF n e deriving Functor
data VarF    n e = VarF    n   deriving Functor
data AppF      e = AppF e e    deriving Functor
data AnnF    a e = AnnF a e    deriving Functor

$(eadtPattern 'LambdaF "Lambda")
$(eadtPattern 'VarF    "Var")
$(eadtPattern 'AppF    "App")
$(eadtPattern 'AnnF    "Ann")

type Expr n    = EADT '[LambdaF n, VarF n, AppF]
type AExpr a n = EADT '[LambdaF n, VarF n, AppF, AnnF a]

class PrettyPrint f where
   prettyPrint' :: f String -> String

instance Show n => PrettyPrint (VarF n) where
   prettyPrint' (VarF n) = show n

instance Show n => PrettyPrint (LambdaF n) where
   prettyPrint' (LambdaF n e) = mconcat ["\\",show n,".",e]

instance PrettyPrint AppF where
   prettyPrint' (AppF e1 e2) = mconcat ["(",e1,") (",e2,")"]

instance Show a => PrettyPrint (AnnF a) where
   prettyPrint' (AnnF a e) = mconcat ["{",show a,"} ",e]

prettyPrint ::
   ( Functor (VariantF xs)
   , BottomUp PrettyPrint xs String String
   ) => EADT xs -> String
prettyPrint e = bottomUp (toBottomUp @PrettyPrint prettyPrint') e

sampleDouble :: Expr String
sampleDouble = Lambda "x" (Var "+" `App` Var "x" `App` Var "x")

sampleAnn :: AExpr String String
sampleAnn = Ann "Double its input" (liftEADT sampleDouble)
