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

import Debug.Trace

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

---------------------------------
-- Helper Functions
---------------------------------
localState :: (GT -> GT) -> InferenceRes -> InferenceRes
localState f st = get >>= \s -> put (f s) >> st >>= \a -> put s >> pure a

typeUniverse :: GlobalInfo -> [T]
typeUniverse gi = [NullT, IntT] ++ (M.keys (dt gi)) 

when2 :: Monad m => Bool -> a -> m a -> m a
when2 c d ma = if c then ma else pure d

resetFail :: GT -> InferenceRes -> InferenceRes -> InferenceRes
resetFail gt inf cont = inf >>= \r -> if r then cont else put gt >> pure False

chainResetFail :: GT -> [InferenceRes] -> InferenceRes
chainResetFail gt irs = foldM (\r ir -> when2 r r $ ir) True irs >>= \r -> when2 (not r) r (put gt >> pure r)

chainResetFailOR :: GT -> [[InferenceRes]] -> InferenceRes
chainResetFailOR gt infs = foldM single False infs
    where
      single r inf = when2 (not r) r (chainResetFail gt inf)

proveAND :: [InferType] -> InferenceRes
proveAND infs = do
  gt <- get 
  r <- foldM single True infs
  if r then pure r else const False <$> put gt
  where
    single r it = if r then infer it else pure r

proveOR :: [[InferType]] -> InferenceRes
proveOR infs = foldM single False infs
    where
      single r inf = when2 (not r) r (proveAND inf)

---------------------------------
-- Inference Alg.
---------------------------------
infer :: Inference
-- (t-num)
infer (TypeOf (E (ValExpr (Number n))) IntT) = pure True

-- (t-null)
-- infer (TypeOf (E (ValExpr NULL)) NullT) = pure True

-- -- (t-id)
-- infer (TypeOf (E (NameExpr (ID id))) t) = do
--   gt <- get
--   case gt id of
--     TBOTTOM -> const True <$> put (extendGT gt (singleGT id t))
--     t'      -> pure (t == t')

-- -- (t-call)
-- infer (TypeOf (E (CallExpr id args)) t0) = do
--   gt <- get
--   case gt id of
--     FuncT targs rt -> if rt == t0 then do
--       proveAND $ [TypeOf (E a) t | (a, t) <- zip args targs] else pure False
      
--     _              -> pure False

-- -- (t-plus)
-- infer (TypeOf (E (AddExpr e1 e2)) IntT) = proveAND [TypeOf (E e1) IntT, TypeOf (E e2) IntT]

-- -- (t-print)
-- infer (TypeOf (E (PrintExpr e)) IntT) = infer (TypeOf (E e) IntT)

-- -- (t-new)
-- infer (TypeOf exp@(E (NewExpr id)) nt@(NewTypeT id')) = do
--   gi <- ask
--   r <- pure (id == id' && M.member nt (dt gi))
--   printLine $ "New: " ++ show exp ++ " <- " ++ show nt ++ ": " ++ show r
--   pure r

-- -- (t-load)
-- infer (TypeOf (E (NameExpr (DeRef e id))) t) = do
--   printLine "Load"
--   gi <- ask
--   gt <- get
--   chainResetFailOR gt (M.keys (dt gi) >>= \tid -> [inf gi tid])
--   where
--     inf gi tid = [pure (t == ((dt gi) M.! tid) id), infer (TypeOf (E (NameExpr e)) tid)]

-- -- (t-store)
-- infer (TypeOf exp@(S (AssignStmt (DeRef e id) lhs)) UnitT) = do
--   printLine $ "Store: " ++ show exp
--   gi <- ask
--   gt <- get
--   proveOR (M.keys (dt gi) >>= \tid -> [inf gi tid])
--   where
--     inf gi tid = let t = ((dt gi) M.! tid) id in
--                  trace (id ++ " <- " ++ show t) [TypeOf (E (NameExpr e)) tid, Assignable (E lhs) t]

-- -- (t-assign)
-- infer (TypeOf (S (AssignStmt (ID id) e)) UnitT) = do
--     gi <- ask
--     printLine $ "Assign with universe: " ++ show (typeUniverse gi)
--     proveOR [[TypeOf (E $ NameExpr $ ID id) t, Assignable (E e) t] | t <- typeUniverse gi]
      
-- -- (t-block)
-- infer (TypeOf (S (BlockStmt s)) t) = infer (TypeOf (S s) t)

-- -- (t-seq)
-- infer (TypeOf (S (SeqStmt s1 s2)) t) = proveAND [TypeOf (S s1) UnitT, TypeOf (S s2) t]

-- -- (t-skip)
-- infer (TypeOf (S (SkipStmt)) UnitT) = pure True

-- -- (t-if)
-- infer (TypeOf (S (IfThenElseStmt e s1 s2)) t) = proveAND [TypeOf (E e) t, TypeOf (S s1) t, TypeOf (S s2) t]

-- -- (t-while)
-- infer (TypeOf (S (WhileStmt e s)) UnitT) = proveAND [TypeOf (E e) IntT, TypeOf (S s) UnitT]

-- -- (t-return)
-- infer (TypeOf (S (ReturnStmt e)) t) = infer (TypeOf (E e) t)

-- -- (t-stmtprog)
-- infer (TypeOf (P (StmtProg s)) t) = infer (TypeOf (S s) t)

-- -- (t-proc)
-- infer (TypeOf (P (DeclProg (ProcDecl id tb s) p)) t) = do
--   gi <- ask
--   gt <- get
--   chainResetFailOR gt (typeUniverse gi >>= pure . inf)
--   where
--     inf t0 = [infer (TypeOf (E (NameExpr (ID id))) (FuncT (mapT . snd <$> tb) t0))
--              ,localState (flip bindGT tb) (infer (TypeOf (S s) t0))
--              ,infer (TypeOf (P p) t)]
             
-- -- (t-dataty)
-- infer (TypeOf (P (DeclProg (NewTypeDecl _ _) p)) t) = infer (TypeOf (P p) t)

-- -- (assignable-bottom)
-- infer exp@(Assignable _ TBOTTOM) = pure False

-- -- (assignable-null)
-- infer exp@(Assignable (E e) NullT) = do
--   gi <- ask
--   r <- proveOR (M.keys (dt gi) >>= \tid -> [[TypeOf (E e) tid]])
--   printLine $ "Assignable: " ++ show exp ++ " <- NullT: " ++ show r
--   pure r

-- -- (assignable-ty)
-- infer (Assignable exp@(E e) t) = do
--   r <- infer (TypeOf (E e) t)
--   printLine $ "Assignable: " ++ show exp ++ " <- " ++ show t ++ ": " ++ show r
--   pure r

-- (t-fail)
infer i = do
  printLine $ "No Match: " ++ show i
  pure False
