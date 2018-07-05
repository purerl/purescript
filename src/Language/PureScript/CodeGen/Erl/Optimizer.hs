-- |
-- This module optimizes code in the simplified-Erlang intermediate representation.
--
-- The following optimizations are supported:
--
--  * Inlining of (>>=) and ret for the Eff monad
--
module Language.PureScript.CodeGen.Erl.Optimizer (optimize) where

import Prelude.Compat

import Control.Monad.Supply.Class (MonadSupply)

import Language.PureScript.CodeGen.Erl.AST
import Language.PureScript.CodeGen.Erl.Optimizer.MagicDo
import Language.PureScript.CodeGen.Erl.Optimizer.Blocks
import Language.PureScript.CodeGen.Erl.Optimizer.Common
import Language.PureScript.CodeGen.Erl.Optimizer.Inliner
import Language.PureScript.CodeGen.Erl.Optimizer.Guards

-- |
-- Apply a series of optimizer passes to simplified Javascript code
--

optimize :: MonadSupply m => Erl -> m Erl
optimize erl = pure erl
  -- do
  -- erl' <- untilFixedPoint (pure . tidyUp . applyAll
  --   [ 
  --   --   inlineCommonValues
  --   -- , inlineCommonOperators
  --   -- , evaluateIifes
  --   ]) erl
  -- -- untilFixedPoint (pure . tidyUp)
  -- --   =<< untilFixedPoint (return . magicDo')
  --   -- =<< untilFixedPoint (return . magicDo) 
  -- pure erl'

  where
  tidyUp :: Erl -> Erl
  tidyUp = applyAll
    [ collapseNestedBlocks
    , inlineSimpleGuards
    ]

untilFixedPoint :: (Monad m, Eq a) => (a -> m a) -> a -> m a
untilFixedPoint f = go
  where
  go a = do
   a' <- f a
   if a' == a then return a' else go a'
