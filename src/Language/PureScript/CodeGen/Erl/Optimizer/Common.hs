module Language.PureScript.CodeGen.Erl.Optimizer.Common where

import Prelude.Compat
import Data.Text (Text)

import Data.Maybe (fromMaybe)

import Language.PureScript.PSString (PSString)
import Language.PureScript.CodeGen.Erl.AST
import Language.PureScript.CodeGen.Erl.Common (atomPS)

import Control.Monad (when)
import Control.Monad.State (State, put, runState)

isFn :: (Text, Text) -> Erl -> Bool
isFn (moduleName, fnName) (EApp (EAtomLiteral (Atom (Just x) y)) []) =
  x == moduleName && y == fnName
isFn _ _ = False

isDict :: (Text, PSString) -> Erl -> Bool
isDict (moduleName, dictName) (EApp (EAtomLiteral (Atom (Just x) y)) []) = x == moduleName && y == atomPS dictName
isDict _ _ = False

isUncurriedFn :: (Text, PSString) -> Erl -> Bool
isUncurriedFn (moduleName, dictName) (EAtomLiteral (Atom (Just x) y)) = x == moduleName && y == atomPS dictName
isUncurriedFn _ _ = False

applyAll :: [a -> a] -> a -> a
applyAll = foldl1 (.)

-- Check if var x occurs in expression
occurs :: Text -> Erl -> Bool
occurs x =
  snd . flip runState False . everywhereOnErlTopDownM go 
  where
  go :: Erl -> State Bool Erl
  go e@(EVar x') | x == x' = do
    put True
    pure e
  go e = pure e

-- Check if var x is (possibly) rebound in expression
isRebound :: Text -> Erl -> Bool
isRebound x =
  snd . flip runState False . everywhereOnErlTopDownM go 
  where
  go :: Erl -> State Bool Erl
  go e@(EFun _ x' _) | x == x' = do
    put True
    pure e
  go e@(EFunFull _ args) = do
    when (any matchBinder args) $ put True
    pure e
  go e@(EVarBind x' _) | x == x' = do
    put True
    pure e
  go e@(ECaseOf _ binds) = do
    when (any (matchCaseBinder . fst) binds) $ put True
    pure e
  go e = pure e

  matchBinder (EFunBinder es _, _) = any (\z -> z == EVar x) es || any (not . isVar) es

  matchCaseBinder (EBinder (EVar x')) = x == x'
  matchCaseBinder (EGuardedBinder (EVar x') _) = x == x'
  matchCaseBinder _ = True -- Conservative

  isVar (EVar _) = True
  isVar _ = False

replaceIdents :: [(Text, Erl)] -> Erl -> Erl
replaceIdents vars = everywhereOnErl replace
  where
  replace v@(EVar var) = fromMaybe v $ lookup var vars
  replace other = other