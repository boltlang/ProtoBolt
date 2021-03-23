{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE FlexibleInstances #-}

module Language.Bolt.Infer where

import Control.Monad (replicateM)
import Control.Monad.State
import Control.Monad.Writer
import Control.Monad.Except
import qualified Data.ByteString as BS
import qualified Data.Map as Map
import qualified Data.Set as Set
import Data.List (nub)

import qualified Language.Bolt.TypeEnv as TypeEnv
import Language.Bolt.Common
import Language.Bolt.Compiler (Diagnostic(..), Compiler, addDiagnostic)
import Language.Bolt.CST
import Language.Bolt.Type

type Infer a = WriterT (Endo [Constraint]) (StateT InferState Compiler) a

data InferState = InferState {
    typeVarCount :: Int,
    typeEnv :: TypeEnv.TypeEnv
  }

type Constraint = (Type, Type)

type Unifier = (Subst, [Constraint])

type Solve a = Except Diagnostic a

newtype Subst = Subst (Map.Map TVar Type)
  deriving (Eq, Ord, Show, Semigroup, Monoid)

class Substitutable a where
  apply :: Subst -> a -> a
  ftv   :: a -> Set.Set TVar

instance Substitutable Type where

  apply _ t@(TCon _) = t
  apply (Subst s) t@(TVar a) = Map.findWithDefault t a s
  apply s (t1 `TArr` t2) = apply s t1 `TArr` apply s t2

  ftv (TCon _) = Set.empty
  ftv (TVar a) = Set.singleton a
  ftv (t1 `TArr` t2) = ftv t1 `Set.union` ftv t2

instance Substitutable Scheme where

  apply (Subst s) (Forall as t) = Forall as $ apply s' t
    where s' = Subst $ foldr Map.delete s as

  ftv (Forall as t) = ftv t `Set.difference` Set.fromList as

instance Substitutable Constraint where
  apply s (t1, t2) = (apply s t1, apply s t2)
  ftv (t1, t2) = ftv t1 `Set.union` ftv t2

instance Substitutable a => Substitutable [a] where
  apply = map . apply
  ftv = foldr (Set.union . ftv) Set.empty

instance Substitutable TypeEnv.TypeEnv where
  apply s (TypeEnv.TypeEnv tys) = TypeEnv.TypeEnv $ Map.map (apply s) tys
  ftv (TypeEnv.TypeEnv tys) = ftv $ Map.elems tys

runInfer :: TypeEnv.TypeEnv -> Infer Type -> Compiler (Type, [Constraint])
runInfer typeEnv i
  = do (ty, cs) <- flip evalStateT initInfer $ runWriterT i
       return (ty, appEndo cs [])
  where initInfer = InferState {
          typeVarCount = 0,
          typeEnv
        }

inferExpr :: TypeEnv.TypeEnv -> Node -> Compiler Scheme
inferExpr env ex
  = do (ty, cs) <- runInfer env (infer ex)
       case runSolve cs of
         Left err -> addDiagnostic err >> pure (Forall [] TAny)
         Right subst -> return $ closeOver $ apply subst ty

-- | Return the internal constraints used in solving the type of a node
constraints :: TypeEnv.TypeEnv -> Node -> Compiler ([Constraint], Subst, Type, Scheme)
constraints env ex
   = do (ty, cs) <- runInfer env (infer ex)
        case runSolve cs of
          Left err -> do
            addDiagnostic err
            pure (cs, emptySubst, TAny, Forall [] TAny)
          Right subst -> do
            let sc = closeOver $ apply subst ty
            pure (cs, subst, ty, sc)

lookupEnv :: BS.ByteString -> Infer Type
lookupEnv n
  = do env <- gets typeEnv
       case TypeEnv.lookup n env of
         Nothing -> lift $ lift $ addDiagnostic (UnboundVariableError n) >> pure TAny
         Just s  -> instantiate s

letters :: [String]
letters = [1..] >>= flip replicateM ['a'..'z']

fresh :: Infer Type
fresh
  = do s <- get
       put s { typeVarCount = typeVarCount s + 1 }
       return $ TVar $ TV (letters !! typeVarCount s)

instantiate :: Scheme -> Infer Type
instantiate (Forall as t)
  = do as' <- mapM (const fresh) as
       let s = Subst $ Map.fromList $ zip as as'
       return $ apply s t

generalize :: TypeEnv.TypeEnv -> Type -> Scheme
generalize env t = Forall as t
  where as = Set.toList $ ftv t `Set.difference` ftv env

normalize :: Scheme -> Scheme
normalize (Forall  _ body) = Forall (map snd ord) (normtype body)
  where ord = zip (nub $ fv body) (map TV letters)
        fv (TVar a) = [a]
        fv (TArr a b) = fv a ++ fv b
        fv (TCon _) = []
        normtype (TArr a b) = TArr (normtype a) (normtype b)
        normtype (TCon a) = TCon a
        normtype (TVar a)
          = case lookup a ord of
              Just x -> TVar x
              Nothing -> error "type variable not in signature"

closeOver :: Type -> Scheme
closeOver = normalize . generalize TypeEnv.empty

addConstraint :: Constraint -> Infer ()
addConstraint c
  = tell $ Endo ([c]<>)

forwardDeclare :: TypeEnv.TypeEnv -> Node -> Infer TypeEnv.TypeEnv

forwardDeclare env FunctionDeclaration { name = Identifier { text }, params, body }
  = do tv <- fresh
       return $ TypeEnv.extend (text, Forall [] tv) env

forwardDeclare env _ = return env

inferBindings :: TypeEnv.TypeEnv -> Type -> Node -> Infer TypeEnv.TypeEnv

inferBindings env valTy BindPattern { name = Identifier { text } }
  = return $ TypeEnv.singleton text $ Forall [] valTy

infer :: Node -> Infer Type

infer StringLiteral {}
  = return stringType

infer ReferenceExpression { name = Identifier { text } }
  = lookupEnv text

infer FunctionExpression { params, expr }
  = do retTy <- fresh
       newEnv <- foldM addEnv TypeEnv.empty params
       scoped newEnv $ infer expr
       return retTy
  where addEnv env Param { bindings, typeExpr, defaultValue } = do
          tv <- fresh
          case typeExpr of
            Just (_, x) -> do { ty' <- infer x; addConstraint (tv, ty'); }
          case defaultValue of
            Just (_, x) -> do { ty' <- infer x; addConstraint (tv, ty'); }
          inferBindings env tv bindings

scoped :: TypeEnv.TypeEnv -> Infer a -> Infer a
scoped env' m
  = do env <- gets typeEnv
       modify $ \s -> s { typeEnv = TypeEnv.merge env env' }
       res <- m
       modify $ \s -> s { typeEnv = env }
       return res

-- inferTop :: TypeEnv.TypeEnv -> [(BS.ByteString, Node)] -> Compiler TypeEnv.TypeEnv
-- inferTop env [] = Right env
-- inferTop env ((name, ex):xs)
--   = case inferExpr env ex of
--       Left err -> Left err
--       Right ty -> inferTop (TypeEnv.extend (name, ty) env) xs

emptySubst :: Subst
emptySubst = mempty

compose :: Subst -> Subst -> Subst
(Subst s1) `compose` (Subst s2) = Subst $ Map.map (apply (Subst s1)) s2 `Map.union` s1

occursCheck :: Substitutable a => TVar -> a -> Bool
occursCheck a t = a `Set.member` ftv t

bind :: TVar -> Type -> Solve Subst
bind a t | t == TVar a     = return emptySubst
         | occursCheck a t = throwError (InfiniteTypeError a t)
         | otherwise       = return (Subst $ Map.singleton a t)

unifyMany :: [Type] -> [Type] -> Solve Subst
unifyMany [] [] = return emptySubst
unifyMany (t1 : ts1) (t2 : ts2)
  = do su1 <- unify t1 t2
       su2 <- unifyMany (apply su1 ts1) (apply su1 ts2)
       return $ su2 `compose` su1
unifyMany t1 t2
  = error "unexpected amount of types given to unifyMany"

unify :: Type -> Type -> Solve Subst
unify t1 t2 | t1 == t2 = return emptySubst
unify (TVar v) t = v `bind` t
unify t (TVar v) = v `bind` t
unify (TArr t1 t2) (TArr t3 t4) = unifyMany [t1, t2] [t3, t4]
unify t1 t2 = throwError $ UnificationFailError t1 t2

runSolve cs = runExcept $ solver (emptySubst, cs)

solver :: Unifier -> Solve Subst
solver (su, []) = pure su
solver (su, (t1, t2):cs0)
  = do su1 <- unify t1 t2
       solver (su1 `compose` su, apply su1 cs0)

