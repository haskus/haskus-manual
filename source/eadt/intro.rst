==============================================================================
Introduction
==============================================================================

------------------------------------------------------------------------------
Motivating example
------------------------------------------------------------------------------

Suppose we want to encode lambda-calculus using an ADT. We could use the
following one:

.. code:: haskell

   data Expr n -- "n" represents a variable name
      = Lambda n (Expr n)
      | Var n
      | App (Expr n) (Expr n)

We can define a pretty-print operation:

.. code:: haskell

   prettyPrint :: Show n => Expr n -> String
   prettyPrint = \case
      Var n      -> show n
      Lambda n e -> mconcat ["\\",show n,".",prettyPrint e]
      App e1 e2  -> mconcat ["(",prettyPrint e1,") (",prettyPrint e2,")"]

And we can test on an example:

.. code:: haskell

   sampleDouble :: Expr String
   sampleDouble = Lambda "x" (Var "+" `App` Var "x" `App` Var "x")

   > putStrLn (prettyPrint sampleDouble)
   \"x".(("+") ("x")) ("x")

Now suppose that we want to add support for annotations. We can define a new
expression ADT with an additional constructor:

.. code:: haskell

   data AExpr a n -- "n" represents a variable name, "a" represents an annotation
      = ALambda n (AExpr a n)
      | AVar n
      | AApp (AExpr a n) (AExpr a n)
      | Ann a (AExpr a n)

But now we need to rewrite our operations and expressions (such as "prettyPrint"
and "sampleDouble") to handle and to use the constructors of the new expression
ADT:

.. code:: haskell

   prettyPrintA :: (Show n, Show a) => AExpr a n -> String
   prettyPrintA = \case
      AVar n      -> show n
      ALambda n e -> mconcat ["\\",show n,".",prettyPrintA e]
      AApp e1 e2  -> mconcat ["(",prettyPrintA e1,") (",prettyPrintA e2,")"]
      Ann a e     -> mconcat ["{",show a,"} ", prettyPrintA e]


   sampleDoubleA :: AExpr a String
   sampleDoubleA = ALambda "x" (AVar "+" `AApp` AVar "x" `AApp` AVar "x")

   sampleAnnA :: AExpr String String
   sampleAnnA = Ann "Double its input" sampleDouble

Now the problem is that we have two totally independent expression types
(``Expr`` and ``AExpr``) with different operations (``prettyPrint`` vs
``prettyPrintA``) which can't be easily mixed. Moreover to define
``prettyPrintA`` we had to copy-paste ``prettyPrint`` just to add a single case
alternative. Now suppose that we want to add a new function (e.g. to compute
free variables of an expression): should we implement it for ``Expr``, for
``AExpr``, for both?

Finally suppose that we want to add some other constructors: we either get a
combinatorial explosion of ADTs and functions, or we give up on static checking
and use the "largest" ADT (which contains a superset of the constructors of the
others) with some conventions, e.g. comments and runtime assertions such as "at
this point this expression shouldn't contain any annotation" that are not
enforced by the compiler.

------------------------------------------------------------------------------
Motivating example with EADTs
------------------------------------------------------------------------------

The same example with EADTs would be written as follows. First we define the
EADTs:

.. code:: haskell

   import Haskus.Utils.EADT
   import Haskus.Utils.EADT.TH

   data LambdaF n e = LambdaF n e deriving Functor
   data VarF    n e = VarF    n   deriving Functor
   data AppF      e = AppF    e e deriving Functor
   data AnnF    a e = AnnF    a e deriving Functor

   eadtPattern 'LambdaF "Lambda"
   eadtPattern 'VarF    "Var"
   eadtPattern 'AppF    "App"
   eadtPattern 'AnnF    "Ann"

   type Expr    n = EADT '[LambdaF n, VarF n, AppF]
   type AExpr a n = EADT '[LambdaF n, VarF n, AppF, AnnF a]


Then we define the ``prettyPrint`` operation by using type classes:

.. code:: haskell

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

   prettyPrint :: (BottomUp PrettyPrint xs, Functor (VariantF xs)) => EADT xs -> String
   prettyPrint = bottomUp (toBottomUp @PrettyPrint prettyPrint')


We can test it with:

.. code:: haskell

   sampleDouble :: Expr String
   sampleDouble = Lambda "x" (Var "+" `App` Var "x" `App` Var "x")

   sampleAnn :: AExpr String String
   sampleAnn = Ann "Double its input" (liftEADT sampleDouble)

   > putStrLn (prettyPrint sampleDouble)
   \"x".(("+") ("x")) ("x")

   > putStrLn (prettyPrint sampleAnn)
   {"Double its input"} \"x".(("+") ("x")) ("x")

