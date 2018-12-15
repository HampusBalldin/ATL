module Types where

import Control.Monad.Reader
import Control.Monad.Except
import Control.Monad.Identity
import Control.Monad.State
import Control.Monad

import qualified Data.Map as M
import Data.Map (Map)

import ATL
import GlobalInfo
import Environments
import EnvironmentUtils

mappendTypes :: T -> T -> Maybe T
mappendTypes (SecretT t1) t2 = SecretT <$> mappendTypes t1 t2
mappendTypes t1 (SecretT t2) = SecretT <$> mappendTypes t1 t2
mappendTypes t1 t2           = if t1 == t2 then Just t1 else Nothing

printLine l = liftIO  $ putStrLn l

data Inferrable = E Expr
                | S Stmt
                | P Prog
               deriving (Eq, Show)

data InferType = TypeOf Inferrable T
               | Assignable Inferrable T
               deriving (Eq, Show)

type InferenceRes = ReaderT GlobalInfo (StateT GT IO) Bool
type Inference = InferType -> InferenceRes

typeCheck :: Prog -> IO Bool
typeCheck p = evalStateT (runReaderT inf gi) newGT
    where
      gi  = globalInfo p
      inf = infer (TypeOf (P p) IntT)

localState :: (GT -> GT) -> InferenceRes -> InferenceRes
localState f st = get >>= \s -> put (f s) >> st >>= \a -> put s >> pure a

typeUniverse :: GlobalInfo -> [T]
typeUniverse gi = [NullT, IntT] ++ (M.keys (dt gi)) 

when2 :: Monad m => Bool -> a -> m a -> m a
when2 c d ma = if c then ma else pure d

proveAND :: [InferType] -> InferenceRes
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
  printLine "Call"
  gt <- get
  printLine $ "GT ID: " ++ show (gt id)
  case gt id of
    FuncT targs rt -> if rt == t0 then do
      printLine $ "Match: " ++ show (zip args targs)
      proveAND $ [TypeOf (E a) t | (a, t) <- zip args targs] else pure False
    _              -> pure False

-- (t-plus)
infer (TypeOf (E (AddExpr e1 e2)) IntT) = proveAND [TypeOf (E e1) IntT, TypeOf (E e2) IntT]

-- (t-print)
infer (TypeOf (E (PrintExpr e)) IntT) = infer (TypeOf (E e) IntT)

-- (t-assign)
infer (TypeOf (S (AssignStmt (ID id) e)) UnitT) = do
    gi <- ask
    foldM single False (typeUniverse gi)
    where
      single r t = if not r then proveAND (prog t) else pure True
      prog t = [TypeOf (E $ NameExpr $ ID id) t, Assignable (E e) t]

-- (t-seq)
infer (TypeOf (S (SeqStmt s1 s2)) t) = proveAND [TypeOf (S s1) UnitT, TypeOf (S s2) t]

-- (t-return)
infer (TypeOf (S (ReturnStmt e)) t) = infer (TypeOf (E e) t)

-- (t-stmtprog)
infer (TypeOf (P (StmtProg s)) t) = infer (TypeOf (S s) t)

-- (t-proc)
infer (TypeOf (P (DeclProg (ProcDecl id tb s) p)) t) = do
  gi <- ask
  r <- foldM single False (typeUniverse gi)
  when2 r r $ do
  infer (TypeOf (P p) t)
  where
    ft = FuncT (mapT . snd <$> tb)
    single r t0 = if not r then do
      gt <- get
      r' <- proveAND [TypeOf (E (NameExpr (ID id))) (ft t0)]
      when2 r' r' $ do
      res <- localState (flip bindGT tb) (infer (TypeOf (S s) t0))
      if not res then put gt >> pure res else pure res
      else pure r

-- (assignable-ty)
infer (Assignable (E e) t) = infer (TypeOf (E e) t)

-- (t-fail)
infer _ = pure False
