.. _variant_do_notation_flowt:

==============================================================================
FlowT (ExceptT-like approach)
==============================================================================

Just like `ExceptT
<https://www.stackage.org/haddock/lts-12.17/transformers-0.5.5.0/Control-Monad-Trans-Except.html#t:ExceptT>`_
wraps ``Either``, we can use a ``FlowT`` newtype to wraps a ``Variant``.
Compared to the :ref:`"Either+Variant" approach<variant_do_notation_either>`, we
avoid the ``Either`` indirection: the first value of the Variant is considered
as the ``Right`` value and the other ones as the error values.

Example:

.. code:: haskell

   import Haskus.Utils.Variant.Flow

   import Prelude hiding (head,lookup)
   import qualified Prelude
   import Text.Read

   data ParseError = ParseError deriving Show

   parse :: String -> Flow '[ParseError] Integer
   parse s = case readMaybe s of
      Just i  -> pure i
      Nothing -> throwE ParseError


   data HeadError = ListWasEmpty deriving Show

   head :: [a] -> Flow '[HeadError] a
   head []    = throwE ListWasEmpty
   head (x:_) = pure x

   data LookupError k = KeyWasNotPresent k deriving Show

   lookup :: Eq k => k -> [(k,v)] -> Flow '[LookupError k] v
   lookup k vs = case Prelude.lookup k vs of
      Just v  -> pure v
      Nothing -> throwE (KeyWasNotPresent k)


   foo :: String -> Flow '[ParseError, LookupError Char, HeadError] Integer
   -- foo :: forall es.
   --    ('[HeadError,ParseError,LookupError Char] :<< es
   --    ) => String -> Flow es Integer
   foo str = do
      c <- liftFlowT $ head str
      r <- liftFlowT $ lookup c codeMap
      liftFlowT $ parse (r ++ tail str)

      where
         codeMap :: [(Char, String)]
         codeMap = [ ('x', "0x")
                   , ('d', "")
                   ]

Test:

.. code:: haskell

   > runFlow (foo "d10")
   V @Integer 10

   > runFlow (foo "x10")
   V @Integer 16

   > runFlow (foo "u10")
   V @(LookupError Char) (KeyWasNotPresent 'u')

   > runFlow (foo "")
   V @HeadError ListWasEmpty

   > runFlow (foo "d10X")
   V @ParseError ParseError

   > foo "" `catchE` (\ListWasEmpty -> success 42) :: Flow '[ParseError,LookupError Char] Integer
   FlowT (Identity V @Integer 42)

