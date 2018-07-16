{-# LANGUAGE ScopedTypeVariables #-}

-- |
-- Data types for the intermediate simplified-Erlang AST
--
module Language.PureScript.CodeGen.Erl.AST where

import Prelude.Compat

import Data.Text (Text)

import Control.Monad.Identity
import Control.Arrow (second)

import Language.PureScript.PSString (PSString)

-- |
-- Data type for simplified Erlang expressions
--
data Erl
  -- |
  -- A numeric literal
  --
  = ENumericLiteral (Either Integer Double)
  -- |
  -- A string literal
  --
  | EStringLiteral PSString
  -- |
  -- A char literal
  --
  | ECharLiteral Char
  -- |
  -- An atom literal (possibly qualified a:b)
  --
  | EAtomLiteral Atom
  -- |
  -- A unary operator application
  --
  | EUnary UnaryOperator Erl
  -- |
  -- A binary operator application
  --
  | EBinary BinaryOperator Erl Erl
  -- |
  -- Top-level function definition (over-simplified)
  --
  | EFunctionDef Atom [Text] Erl
  -- TODO not really a separate form. and misused
  | EVarBind Text Erl
  -- |
  -- A variable
  --
  | EVar Text
  -- |
  -- A function reference f/1
  --
  | EFunRef Atom Int
  -- |
  -- A fun definition
  --
  | EFun (Maybe Text) Text Erl
  | EFunFull (Maybe Text) [(EFunBinder, Erl)]
  -- |
  -- Function application
  --
  | EApp Erl [Erl]
  -- |
  -- Block
  --
  | EBlock [Erl]
  -- |
  -- Tuple literal {a, 1, "C"}
  --
  | ETupleLiteral [Erl]

  | EComment Text

  | EMapLiteral [(Atom, Erl)]

  | EMapPattern [(Atom, Erl)]

  | EMapUpdate Erl [(Atom,Erl)]

  | ECaseOf Erl [(EBinder, Erl)]

  | EArrayLiteral [Erl]
  -- |
  -- Attribute including raw text between the parens
  --
  | EAttribute PSString PSString

  deriving (Show, Eq)

data EFunBinder
 = EFunBinder [Erl] (Maybe Guard)

   deriving (Show, Eq)

data EBinder
  = EBinder Erl -- TODO split out literals?
  | EGuardedBinder Erl Guard

  deriving (Show, Eq)

data Guard
  = Guard Erl
  deriving (Show, Eq)

-- | Possibly qualified atom
-- TODO : This is not really an atom, each part is an atom.
data Atom
  = Atom (Maybe Text) Text
  | AtomPS (Maybe Text) PSString
  deriving (Show, Eq)
-- |
-- Built-in unary operators
--
data UnaryOperator
  -- |
  -- Numeric negation
  --
  = Negate
  -- |
  -- Boolean negation
  --
  | Not
  -- |
  -- Bitwise negation
  --
  | BitwiseNot
  -- |
  -- Numeric unary \'plus\'
  --
  | Positive
  deriving (Show, Eq)

-- |
-- Built-in binary operators
--
data BinaryOperator
  -- |
  -- Numeric addition
  --
  = Add
  -- |
  -- Numeric subtraction
  --
  | Subtract
  -- |
  -- Numeric multiplication
  --
  | Multiply
  -- |
  -- Numeric division (float)
  --
  | FDivide
  -- |
  -- Numeric division (integer)
  --
  | IDivide
  -- |
  -- Remainder
  --
  | Remainder
  -- |
  -- Generic equality test
  --
  | EqualTo
  -- |
  -- Generic inequality test
  --
  | NotEqualTo
  -- |
  -- Generic identical test
  --
  | IdenticalTo
  -- |
  -- Generic non-identical test
  --
  | NotIdenticalTo
  -- |
  -- Numeric less-than
  --
  | LessThan
  -- |
  -- Numeric less-than-or-equal
  --
  | LessThanOrEqualTo
  -- |
  -- Numeric greater-than
  --
  | GreaterThan
  -- |
  -- Numeric greater-than-or-equal
  --
  | GreaterThanOrEqualTo

  -- |
  -- Boolean and
  --
  | And
  -- |
  -- Boolean or
  --
  | Or
  -- |
  -- Boolean short-circuit and
  --
  | AndAlso
  -- |
  -- Boolean short-circuit or
  --
  | OrElse
  -- |
  -- Boolean xor
  --
  | XOr
  -- |
  -- Bitwise and
  --
  | BitwiseAnd
  -- |
  -- Bitwise or
  --
  | BitwiseOr
  -- |
  -- Bitwise xor
  --
  | BitwiseXor
  -- |
  -- Bitwise left shift
  --
  | ShiftLeft
  -- |
  -- Bitwise right shift
  --
  | ShiftRight
  -- -- |
  -- -- Bitwise right shift with zero-fill
  -- --
  -- | ZeroFillShiftRight
  deriving (Show, Eq)

everywhereOnErl :: (Erl -> Erl) -> Erl -> Erl
everywhereOnErl f = go
  where
  go :: Erl -> Erl
  go (EUnary op e) = f $ EUnary op (go e)
  go (EBinary op e1 e2) = f $ EBinary op (go e1) (go e2)
  go (EFunctionDef a ss e) = f $ EFunctionDef a ss (go e)
  go (EVarBind x e) = f $ EVarBind x (go e)
  go (EFun x s e) = f $ EFun x s (go e)
  go (EFunFull fname args) = f $ EFunFull fname $ map (second go) args
  go (EApp e es) = f $ EApp (go e) (map go es)
  go (EBlock es) = f $ EBlock (map go es)
  go (ETupleLiteral es) = f $ ETupleLiteral (map go es)
  go (EMapLiteral binds) = f $ EMapLiteral $ map (second go) binds
  go (EMapPattern binds) = f $ EMapPattern $ map (second go) binds
  go (EMapUpdate e binds) = f $ EMapUpdate (go e) $ map (second go) binds
  go (ECaseOf e binds) = f $ ECaseOf (go e) $ map (second go) binds
  go (EArrayLiteral es) = f $ EArrayLiteral (map go es)
  go other = f other

everywhereOnErlTopDown :: (Erl -> Erl) -> Erl -> Erl
everywhereOnErlTopDown f = runIdentity . everywhereOnErlTopDownM (Identity . f)

everywhereOnErlTopDownM :: forall m. (Monad m) => (Erl -> m Erl) -> Erl -> m Erl
everywhereOnErlTopDownM f = f >=> go
  where
  f' = f >=> go

  fargs :: [(x, Erl)] -> m [(x, Erl)]
  fargs = traverse (sequence . second f')

  go (EUnary op e) = EUnary op <$> f' e
  go (EBinary op e1 e2) = EBinary op <$> f' e1 <*> f' e2
  go (EFunctionDef a ss e) = EFunctionDef a ss <$> f' e
  go (EVarBind x e) = EVarBind x <$> f' e
  go (EFun x s e) = EFun x s <$> f' e
  go (EFunFull fname args) = EFunFull fname <$> fargs args
  go (EApp e es) = EApp <$> f' e <*> traverse f' es
  go (EBlock es) = EBlock <$> traverse f' es
  go (ETupleLiteral es) = ETupleLiteral <$> traverse f' es
  go (EMapLiteral binds) = EMapLiteral <$> fargs binds
  go (EMapPattern binds) = EMapPattern <$> fargs binds
  go (EMapUpdate e binds) = EMapUpdate <$> f' e <*> fargs binds
  go (ECaseOf e binds) = ECaseOf <$> f' e <*> fargs binds
  go (EArrayLiteral es) = EArrayLiteral <$> traverse f' es
  go other = f other
