-- |
-- This module implements the "Magic Do" optimization, which inlines calls to return
-- and bind for the Eff monad, as well as some of its actions.
--
module Language.PureScript.CodeGen.Erl.Optimizer.MagicDo (magicDo, magicDo') where

import Prelude.Compat

import Data.Text (Text)

import Language.PureScript.CodeGen.Erl.AST
import Language.PureScript.CodeGen.Erl.Optimizer.Common
import Language.PureScript.Options
import qualified Language.PureScript.Constants as C
import qualified Language.PureScript.CodeGen.Erl.Constants as EC

magicDo :: Erl -> Erl
magicDo = magicDo'' C.eff C.effDictionaries

magicDo' :: Erl -> Erl
magicDo' = magicDo'' C.effect C.effectDictionaries

magicDo'' :: Text -> C.EffectDictionaries -> Erl -> Erl
magicDo'' effectModule C.EffectDictionaries{..} = everywhereOnErl undo . everywhereOnErlTopDown convert
  where
  -- The name of the function block which is added to denote a do block
  fnName = "__do"
  -- Desugar monomorphic calls to >>= and return for the Eff monad

  convert :: Erl -> Erl
  -- Desugar pure
  convert (EApp (EApp pure' [val]) []) | isPure pure' = val
  -- Desugar >>
  convert (EApp (EApp bind [m]) [EFunFull Nothing [(EFunBinder [] Nothing, e)]]) | isBind bind =
    EFunFull (Just fnName) [(EFunBinder [] Nothing, EBlock (EApp m [] : [ EApp e [] ]))]
  -- Desugar >>=
  convert (EApp (EApp bind [m]) [EFun Nothing "__unused" e]) | isBind bind =
    EFunFull (Just fnName) [(EFunBinder [] Nothing, EBlock (EApp m [] : [ EApp e [] ]))]
  convert (EApp (EApp bind [m]) [EFun Nothing var e]) | isBind bind =
    EFunFull (Just fnName) [(EFunBinder [] Nothing, EBlock (EVarBind var (EApp m []) : [ EApp e [] ]))]
  convert (EApp (EApp bind [m]) [EFunFull Nothing [(EFunBinder [EVar var] Nothing, e)]]) | isBind bind =
    EFunFull (Just fnName) [(EFunBinder [] Nothing, EBlock (EVarBind var (EApp m []) : [ EApp e [] ]))]


  convert other = other
  -- Check if an expression represents a monomorphic call to >>= for the Eff monad
  isBind (EApp fn [dict]) | isDict (effectModule, edBindDict) dict && isBindPoly fn = True
  isBind _ = False
  -- Check if an expression represents a monomorphic call to pure or return for the Eff applicative
  isPure (EApp fn [dict]) | isDict (effectModule, edApplicativeDict) dict && isPurePoly fn = True
  isPure _ = False
  -- Check if an expression represents the polymorphic >>= function
  isBindPoly = isFn (EC.controlBind, C.bind)
  -- Check if an expression represents the polymorphic pure or return function
  isPurePoly = isFn (EC.controlApplicative, C.pure')

  -- Remove __do function applications which remain after desugaring
  undo :: Erl -> Erl
  undo (EApp (EFunFull (Just ident) [(EFunBinder [] Nothing, body)]) []) | ident == fnName = body
  undo other = other
