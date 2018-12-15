module Types where

import Control.Monad.Reader
import Control.Monad.Except
import Control.Monad.Identity
import Control.Monad.State
import Control.Monad

import qualified Data.Map as M
import Data.Map (Map)

import ATL
import Environments
import EnvironmentUtils

mappendTypes :: T -> T -> Maybe T
mappendTypes (SecretT t1) t2 = SecretT <$> mappendTypes t1 t2
mappendTypes t1 (SecretT t2) = SecretT <$> mappendTypes t1 t2
mappendTypes t1 t2           = if t1 == t2 then Just t1 else Nothing

data Inferrable = E Expr
                | S Stmt
                | P Prog
                deriving (Eq, Show)

data InferType = TypeOf Inferrable T
               | Assignable Inferrable T
               deriving (Eq, Show)

type Inference = InferType -> StateT GT Identity Bool

localState :: Monad m => (s -> s) -> StateT s m a -> StateT s m a
localState f st = do
  s <- get
  a <- withStateT f st
  put s
  pure a

  

typeUniverse :: [T]
typeUniverse = [NullT, IntT]

evalInfer :: InferType -> Bool
evalInfer inf = runIdentity (evalStateT (infer inf) newGT)

runInferWith :: GT -> InferType -> (Bool, GT)
runInferWith gt inf = runIdentity (runStateT (infer inf) gt)

proveAND :: [InferType] -> StateT GT Identity Bool
proveAND infs = do
  gt <- get 
  r <- foldM single True infs
  if r then pure r else const False <$> put gt
  where
    single r it = if r then infer it else pure r

infer :: Inference
-- (t-num)
infer (TypeOf (E (ValExpr (Number n))) IntT) = pure True

-- (t-null)
infer (TypeOf (E (ValExpr NULL)) NullT)      = pure True

-- (t-id)
infer (TypeOf (E (NameExpr (ID id))) t) = do
  gt <- get
  case gt id of
    TBOTTOM -> const True <$> put (extendGT gt (singleGT id t))
    t'      -> pure (t == t')

-- (t-call)
infer (TypeOf (E (CallExpr id args)) t0) = do
  gt <- get
  case gt id of
    FuncT targs rt -> if rt == t0 then proveAND $ [TypeOf (E a) t | (a, t) <- zip args targs] else pure False
    _              -> pure False

-- (t-plus)
infer (TypeOf (E (AddExpr e1 e2)) IntT) = proveAND [TypeOf (E e1) IntT, TypeOf (E e2) IntT]

-- (t-assign)
infer (TypeOf (S (AssignStmt (ID id) e)) UnitT) = foldM single False typeUniverse 
    where
      single r t = if not r then proveAND (prog t) else pure True
      prog t = [TypeOf (E $ NameExpr $ ID id) t, Assignable (E e) t]

-- (t-seq)
infer (TypeOf (S (SeqStmt s1 s2)) t) = proveAND [TypeOf (S s1) UnitT, TypeOf (S s2) t]

-- (t-stmtprog)
infer (TypeOf (P (StmtProg s)) UnitT) = infer (TypeOf (S s) UnitT)

-- (t-proc)
-- infer (TypeOf (P (DeclProg (ProcDecl id tb s) p)) t) = do
--   infer (TypeOf (P p) t)
--   infer (TypeOf (E (NameExpr (ID id))) (FuncT (mapT . snd <$> tb) t0))
--   localState (\gt -> gt) (infer (TypeOf (S s) t))

-- (assignable-ty)
infer (Assignable (E e) t) = infer (TypeOf (E e) t)

-- (t-fail)
infer _ = pure False

  
  
