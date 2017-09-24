{-# LANGUAGE GADTs #-}

-- |
-- This module generates code in the simplified Erlang intermediate representation from Purescript code
--
module Language.PureScript.CodeGen.Erl
  ( module AST
  , moduleToErl
  ) where

import Prelude.Compat

import Language.PureScript.CodeGen.Erl.AST as AST

import qualified Data.Text as T
import Data.Traversable
import Data.Foldable
import Data.Monoid
import qualified Data.Map as M
import Data.Maybe (fromMaybe, mapMaybe)
import Control.Monad.Error.Class (MonadError(..))

import Control.Arrow (first, second)
import Control.Monad.Reader (MonadReader(..))

import Control.Monad.Supply.Class

import Language.PureScript.CoreFn hiding (moduleExports)
import Language.PureScript.Errors (MultipleErrors, rethrow, addHint, ErrorMessageHint(..), SimpleErrorMessage(..), errorMessage, rethrowWithPosition)
import Language.PureScript.Options
import Language.PureScript.Names
import Language.PureScript.Types
import Language.PureScript.Environment as E
import qualified Language.PureScript.Constants as C
import Language.PureScript.Traversals (sndM)

import Language.PureScript.CodeGen.Erl.Common
import Language.PureScript.CodeGen.Erl.Optimizer

import Debug.Trace

freshNameErl :: (MonadSupply m) => m T.Text
freshNameErl = fmap (("_@" <>) . T.pack . show) fresh


identToTypeclassCtor :: Ident -> Atom
identToTypeclassCtor a = Atom Nothing (runIdent a)

qualifiedToTypeclassCtor :: Qualified Ident -> Atom
qualifiedToTypeclassCtor (Qualified (Just mn) ident) = Atom (Just $ atomModuleName mn PureScriptModule) (runIdent ident)
qualifiedToTypeclassCtor (Qualified  Nothing ident) = Atom Nothing (runIdent ident)

isTopLevelBinding :: Qualified t -> Bool
isTopLevelBinding (Qualified (Just _) _) = True
isTopLevelBinding (Qualified Nothing _) = False

tyArity :: Type -> Int
tyArity (TypeApp (TypeApp fn _) ty) | fn == E.tyFunction = 1 + tyArity ty
tyArity (ForAll _ ty _) = tyArity ty
tyArity (ConstrainedType _ ty) = 1 + tyArity ty
tyArity _ = 0

-- |
-- Generate code in the simplified Erlang intermediate representation for all declarations in a
-- module.
--
moduleToErl :: forall m .
    (Monad m, MonadReader Options m, MonadSupply m, MonadError MultipleErrors m)
  => E.Environment
  -> Module Ann
  -> [(T.Text, Int)]
  -> m ([T.Text], [Erl])
moduleToErl env (Module _ mn _ _ foreigns decls) foreignExports =
  rethrow (addHint (ErrorInModule mn)) $ do
    res <- traverse topBindToErl decls
    let (exports, erlDecls) = biconcat $ res <> map reExportForeign foreigns
    optimized <- traverse optimize erlDecls
    traverse_ checkExport foreigns

    let attributes = findAttributes decls

    return (map (\(a,i) -> runAtom a <> "/" <> T.pack (show i)) exports, attributes ++ optimized)
  where

  findAttributes :: [Bind Ann] -> [Erl]
  findAttributes expr = map (uncurry EAttribute) $ mapMaybe getAttribute $ concatMap onBind expr
    where
      getAttribute (TypeApp (TypeApp (TypeConstructor (Qualified (Just mn) (ProperName "Attribute"))) (TypeLevelString a)) (TypeLevelString b))
        = Just (a,b)
      getAttribute _ = Nothing

      getType ident = (\(t, _, _) -> t) <$> M.lookup (Qualified (Just mn) ident) (E.names env)

      onRecBind ((_, ident), _) = getType ident
      onBind (NonRec _ ident val) = mapMaybe id [ getType ident ]
      onBind (Rec vals) = mapMaybe onRecBind vals

  biconcat :: [([a], [b])] -> ([a], [b])
  biconcat x = (concatMap fst x, concatMap snd x)

  arities :: M.Map (Qualified Ident) Int
  arities = M.map (\(t, _, _) -> tyArity t) $ E.names env

  reExportForeign :: (Ident, Type) -> ([(Atom,Int)], [Erl])
  reExportForeign (ident, _) =
    let arity = exportArity ident
        fullArity = fromMaybe 0 (M.lookup (Qualified (Just mn) ident) arities)
        args = map (\m -> "X" <> T.pack (show m)) [ 1..fullArity ]
        body = EApp (EAtomLiteral $ qualifiedToErl' mn ForeignModule ident) (take arity $ map EVar args)
        body' = curriedApp (drop arity $ map EVar args) body
        fun = curriedLambda body' args
        nameC = curriedName (Atom Nothing $ identToAtomName ident)
        nameUC = uncurriedName (Atom Nothing $ identToAtomName ident)
    in ([(nameC, 0), (nameUC, fullArity)],
        [ EFunctionDef nameC [] fun, EFunctionDef nameUC args body'])

  curriedLambda :: Erl -> [T.Text] -> Erl
  curriedLambda = foldr (EFun Nothing)

  exportArity :: Ident -> Int
  exportArity ident = fromMaybe 0 $ findExport $ runIdent ident

  checkExport :: (Ident, Type) -> m ()
  checkExport (ident,ty) =
    case (findExport (runIdent ident), tyArity ty) of
      (Just m, n) | m > n ->
        throwError . errorMessage $ InvalidFFIArity mn (runIdent ident) m n
      (Nothing, _) ->
        throwError . errorMessage $ MissingFFIImplementations mn [ident]
      _ -> pure ()

  findExport :: T.Text -> Maybe Int
  findExport n = snd <$> find ((n==) . fst) foreignExports

  topBindToErl :: Bind Ann -> m ([(Atom,Int)], [Erl])
  topBindToErl (NonRec ann ident val) = topNonRecToErl ann ident val
  topBindToErl (Rec vals) = biconcat <$> traverse (uncurry . uncurry $ topNonRecToErl) vals

  topNonRecToErl :: Ann -> Ident -> Expr Ann -> m ([(Atom,Int)], [ Erl ])
  topNonRecToErl _ ident val = do
    erl <- valueToErl val
    let (_, _, _, meta') = extractAnn val
    let ident' = case meta' of
          Just IsTypeClassConstructor -> identToTypeclassCtor ident
          _ -> Atom Nothing $ runIdent ident
    let arity = fromMaybe 0 (M.lookup (Qualified (Just mn) ident) arities)
    let vars = map (\m -> "X" <> T.pack (show m)) [ 1..arity ]
    pure
      ( [ (uncurriedName ident', arity)
        , (curriedName ident', 0)
        ]
      , [ EFunctionDef (uncurriedName ident') vars $ curriedApp (map EVar vars) erl
        , EFunctionDef (curriedName ident') [] erl
        ]
      )

  uncurriedName = id

  curriedName (Atom q t) = Atom q (t <> "@c")
  curriedName x = x

  bindToErl :: Bind Ann -> m [Erl]
  bindToErl (NonRec ann ident val) =
    pure <$> EVarBind (identToVar ident) <$> valueToErl' (Just ident) val
  -- bindToErl (Rec [((ann, ident), val)]) =
  --   pure <$> EVarBind (identToVar ident) <$> valueToErl' (Just ident) val

  -- For recursive bindings F(X) = E1, G(X) = E2, ... we have a problem as the variables are not
  -- in scope until each expression is defined. To avoid lifting to the top level first generate
  -- funs which take a tuple of such funs F'({F', G'}) -> (X) -> E1 etc.
  -- with occurences of F, G replaced in E1, E2 with F'({F',G'})
  -- and then bind these F = F'({F',G'})
  -- TODO: Only do this if there are multiple mutually recursive bindings! Else a named fun works.
  bindToErl (Rec vals) = do
    let vars = (identToVar . snd . fst) <$> vals
        varTup = ETupleLiteral $ EVar <$> (<> "@f") <$> vars
        replaceFun fvar = everywhereOnErl go
          where
            go (EVar f) | f == fvar = EApp (EVar $ f <> "@f") [varTup]
            go e = e

    funs <- forM vals $ \((_, ident), val) -> do
      erl <- valueToErl' Nothing val
      let erl' = foldr replaceFun erl vars
      let fun = EFunFull Nothing [(EFunBinder [varTup] Nothing, erl')]
      pure $ EVarBind (identToVar ident <> "@f") fun
    let rebinds = map (\var -> EVarBind var (EApp (EVar $ var <> "@f") [varTup])) vars
    pure $ funs ++ rebinds

  qualifiedToErl' mn' moduleType ident = Atom (Just $ atomModuleName mn' moduleType) (runIdent ident)

  -- Top level definitions are everywhere fully qualified, variables are not.
  qualifiedToErl (Qualified (Just mn') ident) | mn == mn' = Atom Nothing (runIdent ident)
  qualifiedToErl (Qualified (Just mn') ident) = qualifiedToErl' mn' PureScriptModule ident
  qualifiedToErl _ = error "Invalid qualified identifier"

  qualifiedToVar (Qualified _ ident) = identToVar ident

  valueToErl :: Expr Ann -> m Erl
  valueToErl = valueToErl' Nothing

  valueToErl' :: Maybe Ident -> Expr Ann -> m Erl
  valueToErl' _ (Literal (pos, _, _, _) l) =
    rethrowWithPosition pos $ literalToValueErl l
  valueToErl' _ (Var _ (Qualified (Just (ModuleName [ProperName prim])) (Ident undef))) | prim == C.prim, undef == C.undefined =
    return $ EAtomLiteral $ Atom Nothing C.undefined
  valueToErl' _ (Var _ ident) | isTopLevelBinding ident = return $ EApp (EAtomLiteral $ curriedName $ qualifiedToErl ident) []
  valueToErl' _ (Var _ ident) = return $ EVar $ qualifiedToVar ident

  valueToErl' ident (Abs _ arg val) = do
    ret <- valueToErl val
    return $ EFun (fmap identToVar ident) (identToVar arg) ret

  valueToErl' _ (Accessor _ prop val) = do
    eval <- valueToErl val
    return $ EApp (EAtomLiteral $ Atom (Just "maps") "get") [EAtomLiteral $ AtomPS Nothing prop, eval]

  valueToErl' _ (ObjectUpdate _ o ps) = do
    obj <- valueToErl o
    sts <- mapM (sndM valueToErl) ps
    return $ EMapUpdate obj (map (first (AtomPS Nothing)) sts)

  valueToErl' _ e@App{} = do
    let (f, args) = unApp e []
    args' <- mapM valueToErl args
    case f of
      Var (_, _, _, Just IsNewtype) _ -> return (head args')
      Var (_, _, _, Just (IsConstructor _ fields)) (Qualified _ ident) | length args == length fields ->
        return $ constructorLiteral (runIdent ident) args'
      Var (_, _, _, Just IsTypeClassConstructor) name ->
        return $ curriedApp args' $ EApp (EAtomLiteral $ qualifiedToTypeclassCtor name) []

      Var _ qi@(Qualified q ident)
        | arity <- fromMaybe 0 (M.lookup qi arities)
        , length args == arity
        -> return $ EApp (EAtomLiteral (uncurriedName $ qualifiedToErl qi)) args'

      _ -> curriedApp args' <$> valueToErl f
    where
    unApp :: Expr Ann -> [Expr Ann] -> (Expr Ann, [Expr Ann])
    unApp (App _ val arg) args = unApp val (arg : args)
    unApp other args = (other, args)

  valueToErl' _ (Case _ values binders) = do
    vals <- mapM valueToErl values
    (exprs, binders', newvals) <- bindersToErl vals binders
    let ret = EApp (EFunFull Nothing binders') (vals++newvals)
    pure $ case exprs of
      [] -> ret
      _ -> EBlock (exprs ++ [ret])
  valueToErl' _ (Let _ ds val) = do
    ds' <- concat <$> mapM bindToErl ds
    ret <- valueToErl val
    -- TODO: Rename variables rather than creating temporary scope just for this
    return $ iife (ds' ++ [ret])

  valueToErl' _ (Constructor (_, _, _, Just IsNewtype) _ _ _) = error "newtype ctor"
  valueToErl' _ (Constructor _ _ (ProperName ctor) fields) =
    let createFn =
          let body = constructorLiteral ctor ((EVar . identToVar) `map` fields)
          in foldr (\f inner -> EFun Nothing (identToVar f) inner) body fields
    in pure createFn

  iife exprs = EApp (EFunFull Nothing [(EFunBinder [] Nothing, (EBlock exprs))]) []

  constructorLiteral name args = ETupleLiteral (EAtomLiteral (Atom Nothing (toAtomName name)) : args)

  curriedApp :: [Erl] -> Erl -> Erl
  curriedApp args' body = flip (foldl (\fn a -> EApp fn [a])) args' body

  literalToValueErl :: Literal (Expr Ann) -> m Erl
  literalToValueErl = literalToValueErl' EMapLiteral valueToErl

  literalToValueErl' ::  ([(Atom,Erl)] -> Erl) -> (a -> m Erl) -> Literal a -> m Erl
  literalToValueErl' _ _ (NumericLiteral n) = return $ ENumericLiteral n
  literalToValueErl' _ _ (StringLiteral s) = return $ EStringLiteral s
  literalToValueErl' _ _ (CharLiteral c) = return $ ECharLiteral c
  literalToValueErl' _ _ (BooleanLiteral b) = return $ boolToAtom b
  literalToValueErl' _ f (ArrayLiteral xs) = do
    array <- EArrayLiteral <$> mapM f xs
    pure $ EApp (EAtomLiteral $ Atom (Just "array") "from_list") [array]
  literalToValueErl' mapLiteral f (ObjectLiteral ps) = do
    pairs <- mapM (sndM f) ps
    pure $ mapLiteral $ map (first (AtomPS Nothing)) pairs

  boolToAtom :: Bool -> Erl
  boolToAtom True = EAtomLiteral $ Atom Nothing "true"
  boolToAtom False = EAtomLiteral $ Atom Nothing "false"

  bindersToErl :: [Erl] -> [CaseAlternative Ann] -> m ([Erl], [(EFunBinder, Erl)], [Erl])
  bindersToErl vals cases = do
    res <- mapM caseToErl cases
    let arrayVars = map fst $ concatMap (\(_,_,x) -> x) res
        arrayMatches = map (\(_,_,x) -> x) res
        convBinder (count, binds) (_, binders, arrayMatches) =
          (count + length arrayMatches, binds ++ map go binders)
          where
            go (EFunBinder bs z, e) = (EFunBinder (bs++padBinds count arrayMatches) z, e)
        padBinds n binds = replicate n (EVar "_") ++ (map snd binds) ++ replicate (length arrayVars - n - length binds) (EVar "_")
        binders' = snd $ foldl convBinder (0, []) res

    pure (concatMap (\(x,_,_) -> x) res, binders', arrayVars)
    where
    caseToErl :: CaseAlternative Ann -> m ([Erl], [(EFunBinder, Erl)], [(Erl, Erl)])
    caseToErl (CaseAlternative binders alt) = do
      b' <- mapM (binderToErl' vals) binders
      let (bs, erls) = second concat $ unzip b'
      (es, res) <- case alt of
        Right e -> do
          e' <- valueToErl e
          pure ([], [(EFunBinder bs Nothing, e')])
        Left guards -> first concat <$> unzip <$> mapM (guard bs) guards
      pure (es ++ map ((\f -> f $ (EFunBinder bs Nothing)) . fst) erls, res, map (first EVar . snd) erls)
    guard bs (ge, e) = do
      var <- freshNameErl
      ge' <- valueToErl ge
      let fun = EFunFull Nothing
                  [(EFunBinder bs Nothing, ge'),
                  (EFunBinder (replicate (length bs) (EVar "_")) Nothing, boolToAtom False)]
          cas = EApp fun vals
      e' <- valueToErl e
      pure ([EVarBind var cas], (EFunBinder bs (Just $ Guard $ EVar var), e'))

  binderToErl' :: [Erl] -> Binder Ann -> m (Erl,[(EFunBinder -> Erl,(T.Text, Erl))])
  binderToErl' _ (NullBinder _) = pure (EVar "_", [])
  binderToErl' _ (VarBinder _ ident) = pure (EVar $ identToVar ident, [])
  binderToErl' vals (LiteralBinder _ (ArrayLiteral es)) = do
    x <- freshNameErl
    args' <- mapM (binderToErl' vals) es

    let cas binder = EApp (EFunFull Nothing
                [(binder, EApp (EAtomLiteral $ Atom (Just "array") "to_list") [EVar x]),
                (EFunBinder (replicate (length vals) (EVar "_")) Nothing, EAtomLiteral $ Atom Nothing "nil")]) vals
    var <- freshNameErl

    let arr = EArrayLiteral (map fst args')
    pure (EVar x, ((EVarBind var . cas, (var, arr)) : concatMap snd args'))
  binderToErl' vals (LiteralBinder _ lit) = (,[]) <$> literalToValueErl' EMapPattern (fmap fst . binderToErl' vals) lit
  binderToErl' vals (ConstructorBinder (_, _, _, Just IsNewtype) _ _ [b]) = binderToErl' vals b
  binderToErl' vals (ConstructorBinder _ _ (Qualified _ (ProperName ctorName)) binders) = do
    args' <- mapM (binderToErl' vals) binders
    pure (constructorLiteral ctorName (map fst args'), concatMap snd args')
  binderToErl' vals (NamedBinder _ ident binder) = do
    (e, xs) <- binderToErl' vals binder
    pure (EVarBind (identToVar ident) e, xs)
