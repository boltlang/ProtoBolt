{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE FlexibleInstances #-}

module Language.Bolt.Infer where

import Control.Monad (replicateM)
import Control.Monad.State
import Control.Monad.Reader
import Control.Monad.Except
import qualified Data.ByteString as BS
import qualified Data.Map as Map
import qualified Data.Set as Set
import Data.List (nub)

import qualified Language.Bolt.TypeEnv as TE
import Language.Bolt.Common
import Language.Bolt.AST
import Language.Bolt.Type

data TypeError
  = UnificationFail Type Type
  | InfiniteType TVar Type
  | UnboundVariable BS.ByteString
  | Ambigious [Constraint]
  | UnificationMismatch [Type] [Type]
  deriving (Eq, Show)

type Infer a = (ReaderT TE.TypeEnv (StateT InferState (Except TypeError))) a

data InferState = InferState { count :: Int }

initInfer :: InferState
initInfer = InferState { count = 0 }

type Constraint = (Type, Type)

type Unifier = (Subst, [Constraint])

type Solve a = Except TypeError a

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

instance Substitutable TE.TypeEnv where
  apply s (TE.TypeEnv tys) = TE.TypeEnv $ Map.map (apply s) tys
  ftv (TE.TypeEnv tys) = ftv $ Map.elems tys

runInfer :: TE.TypeEnv -> Infer (Type, [Constraint]) -> Either TypeError (Type, [Constraint])
runInfer env m
  = runExcept $ evalStateT (runReaderT m env) initInfer

inferExpr :: TE.TypeEnv -> AST -> Either TypeError Scheme
inferExpr env ex
  = do (ty, cs) <- runInfer env (infer ex)
       subst <- runSolve cs
       Right $ closeOver $ apply subst ty

constraintsExpr :: TE.TypeEnv -> AST -> Either TypeError ([Constraint], Subst, Type, Scheme)
constraintsExpr env ex
   = do (ty, cs) <- runInfer env (infer ex)
        subst <- runSolve cs
        let sc = closeOver $ apply subst ty
        Right $ (cs, subst, ty, sc)

insertEnv :: (BS.ByteString, Scheme) -> Infer a -> Infer a
insertEnv (n, sc) m
  = local scope m
  where scope e = TE.extend (n, sc) (TE.delete n e)

lookupEnv :: BS.ByteString -> Infer Type
lookupEnv n
  = do env <- ask
       case TE.lookup n env of
         Nothing -> throwError $ UnboundVariable n
         Just s  -> instantiate s

letters :: [String]
letters = [1..] >>= flip replicateM ['a'..'z']

fresh :: Infer Type
fresh
  = do s <- get
       put s { count = count s + 1 }
       return $ TVar $ TV (letters !! count s)

instantiate :: Scheme -> Infer Type
instantiate (Forall as t)
  = do as' <- mapM (const fresh) as
       let s = Subst $ Map.fromList $ zip as as'
       return $ apply s t

generalize :: TE.TypeEnv -> Type -> Scheme
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
closeOver = normalize . generalize TE.empty

infer :: AST -> Infer (Type, [Constraint])

infer (Lit (VInt _))
  = return (intType, [])

infer (Lit (VText _))
  = return (stringType, [])

infer (Ref n)
  = do t <- lookupEnv n
       return (t, [])

infer (Lam x e)
  = do tv <- fresh
       (t, c) <- insertEnv (x, Forall [] tv) (infer e)
       return (tv `TArr` t, c)

infer (Let n e1 e2)
  = do env <- ask
       (t1, c1) <- infer e1
       case runSolve c1 of
        Left err -> throwError err
        Right sub -> do
          let sc = generalize (apply sub env) (apply sub t1)
          (t2, c2) <- insertEnv (n, sc) $ local (apply sub) (infer e2)
          return (t2, c1 ++ c2)

infer (App e1 e2)
  = do (t1, c1) <- infer e1
       (t2, c2) <- infer e2
       tv <- fresh
       return (tv, c1 ++ c2 ++ [(t1, t2 `TArr` tv)])

infer (If p t e)
  = do (t1, c1) <- infer p
       (t2, c2) <- infer t
       (t3, c3) <- infer e
       return (t2, c1 ++ c2 ++ c3 ++ [(t1, boolType), (t2, t3)])

inferTop :: TE.TypeEnv -> [(BS.ByteString, AST)] -> Either TypeError TE.TypeEnv
inferTop env [] = Right env
inferTop env ((name, ex):xs)
  = case inferExpr env ex of
      Left err -> Left err
      Right ty -> inferTop (TE.extend (name, ty) env) xs

emptySubst :: Subst
emptySubst = mempty

compose :: Subst -> Subst -> Subst
(Subst s1) `compose` (Subst s2) = Subst $ Map.map (apply (Subst s1)) s2 `Map.union` s1

runSolve :: [Constraint] -> Either TypeError Subst
runSolve cs = runExcept $ solver st
  where st = (emptySubst, cs)

occursCheck :: Substitutable a => TVar -> a -> Bool
occursCheck a t = a `Set.member` ftv t

bind :: TVar -> Type -> Solve Subst
bind a t | t == TVar a     = return emptySubst
         | occursCheck a t = throwError $ InfiniteType a t
         | otherwise       = return (Subst $ Map.singleton a t)

unifyMany :: [Type] -> [Type] -> Solve Subst
unifyMany [] [] = return emptySubst
unifyMany (t1 : ts1) (t2 : ts2)
  = do su1 <- unify t1 t2
       su2 <- unifyMany (apply su1 ts1) (apply su1 ts2)
       return $ su2 `compose` su1
unifyMany t1 t2 = throwError $ UnificationMismatch t1 t2

unify :: Type -> Type -> Solve Subst
unify t1 t2 | t1 == t2 = return emptySubst
unify (TVar v) t = v `bind` t
unify t (TVar v) = v `bind` t
unify (TArr t1 t2) (TArr t3 t4) = unifyMany [t1, t2] [t3, t4]
unify t1 t2 = throwError $ UnificationFail t1 t2

solver :: Unifier -> Solve Subst
solver (su, cs)
  = case cs of
     [] -> return su
     ((t1, t2):cs0) -> do
       su1 <- unify t1 t2
       solver (su1 `compose` su, apply su1 cs0)

