module Language.PureScript.CodeGen.Erl.Optimizer.Common where

import Prelude.Compat
import Data.Text (Text)

import Data.Monoid ((<>))

import Language.PureScript.PSString (PSString)
import Language.PureScript.CodeGen.Erl.AST
import Language.PureScript.CodeGen.Erl.Common (atomPS)

isFn :: (Text, Text) -> Erl -> Bool
isFn (moduleName, fnName) (EApp (EAtomLiteral (Atom (Just x) y)) []) =
  x == moduleName && y == fnName
isFn _ _ = False

isDict :: (Text, PSString) -> Erl -> Bool
isDict (moduleName, dictName) (EApp (EAtomLiteral (Atom (Just x) y)) []) = x == moduleName && y == (atomPS dictName <> "@c")
isDict _ _ = False

isUncurriedFn :: (Text, PSString) -> Erl -> Bool
isUncurriedFn (moduleName, dictName) (EAtomLiteral (Atom (Just x) y)) = x == moduleName && y == atomPS dictName
isUncurriedFn _ _ = False

applyAll :: [a -> a] -> a -> a
applyAll = foldl1 (.)
