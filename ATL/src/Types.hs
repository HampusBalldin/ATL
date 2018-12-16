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

type InferenceRes = Reader GlobalInfo [[State GT Bool]]
type Inference = InferType -> InferenceRes

-- typeCheck :: Prog -> IO Bool
-- typeCheck p = evalStateT (runReaderT inf gi) newGT
--     where
--       gi  = globalInfo p
--       inf = infer (TypeOf (P p) IntT)

---------------------------------
-- Helper Functions
---------------------------------
-- localState :: (GT -> GT) -> InferenceRes -> InferenceRes
-- localState f st = get >>= \s -> put (f s) >> st >>= \a -> put s >> pure a

typeUniverse :: GlobalInfo -> [T]
typeUniverse gi = [NullT, IntT] ++ (M.keys (dt gi)) 

when2 :: Monad m => Bool -> a -> m a -> m a
when2 c d ma = if c then ma else pure d

-- resetFail :: GT -> InferenceRes -> InferenceRes -> InferenceRes
-- resetFail gt inf cont = inf >>= \r -> if r then cont else put gt >> pure False

-- chainResetFail :: GT -> [InferenceRes] -> InferenceRes
-- chainResetFail gt irs = foldM (\r ir -> when2 r r $ ir) True irs >>= \r -> when2 (not r) r (put gt >> pure r)

-- chainResetFailOR :: GT -> [[InferenceRes]] -> InferenceRes
-- chainResetFailOR gt infs = foldM single False infs
--     where
--       single r inf = when2 (not r) r (chainResetFail gt inf)

testAND :: [[[a]]] -> [[a]]
testAND []      = [[]]
testAND (inf:infs) = pure (++) <*> inf <*> testAND infs

proveAND :: [InferType] -> InferenceRes
proveAND []      = pure [[]]
proveAND (inf:infs) = do
  gi <- ask
  statesi1 <- infer inf
  pure $ pure (++) <*> statesi1 <*> (runReader (proveAND infs) gi)
  
  -- (++) <$> infer inf <*> proveAND infs
testOR :: [[[[a]]]] -> [[a]]
testOR infs = join (testAND <$> infs)

proveOR :: [[InferType]] -> InferenceRes
proveOR infs = do
  gi <- ask
  pure . join $ flip runReader gi . proveAND <$> infs

typeCheck :: Prog -> [Bool]
typeCheck p = solve states
  where
    states = runIdentity $ runReaderT (infer (TypeOf (P p) IntT)) gi
    gi = globalInfo p


testType :: InferType -> [Bool]
testType inf = solve (runReader (infer inf) newGlobalInfo)

chainAND :: [State GT Bool] -> State GT Bool
chainAND []      = pure True
chainAND (s:sts) = do
    gt <- get
    let (b, gt') = runState s gt
    when2 b b $ do
        put gt'
        b' <- chainAND sts
        pure (b && b')

chainOR :: [State GT Bool] -> State GT Bool
chainOR []      = pure False
chainOR (s:sts) = do
    gt <- get
    let (b, gt') = runState s gt
    when2 (not b) b $ do
        put gt'
        b' <- chainOR sts
        pure (b || b')

solve :: [[State GT Bool]] -> [Bool]
solve states = do
  andBlock <- states
  [runIdentity $ evalStateT (chainAND andBlock) newGT]

true, false :: InferenceRes
pure3 = pure.pure.pure
pure4 = pure.pure.pure.pure
true  = pure4 True
false = pure4 False

-- stateInfer :: (GlobalInfo -> GT -> InferenceRes) -> InferenceRes
-- stateInfer f = do
--   gi <- ask
--   let ir = pure $ pure $ 
  
--   false
  -- pure $ pure $ pure $ do
  --   gt <- get
  --   chainOR (chainAND <$> (runReader (f gt gi) gi))

---------------------------------
-- Inference Alg.
---------------------------------
infer :: Inference
-- (t-num)
infer (TypeOf (E (ValExpr (Number n))) IntT) = true

-- (t-null)
infer (TypeOf (E (ValExpr NULL)) NullT) = true

-- -- (t-id)
infer (TypeOf (E (NameExpr (ID id))) t) = pure3 idstate
 where
   idstate = do
     gt <- get
     case gt id of
       TBOTTOM -> const True <$> put (extendGT gt (singleGT id t))
       t'      -> pure (t == t')

-- -- (t-call)
-- infer (TypeOf (E (CallExpr id args)) t0) = stateInfer callstate
--     where
--         callstate gi = do
--             gt <- get
--             case gt id of
--                 FuncT targs rt -> if rt == t0 then do
--                     (proveAND [TypeOf (E a) t | (a, t) <- zip args targs]) else false
--                 _              -> false

-- -- (t-plus)
infer (TypeOf (E (AddExpr e1 e2)) IntT) = proveAND [TypeOf (E e1) IntT, TypeOf (E e2) IntT]

-- -- (t-print)
infer (TypeOf (E (PrintExpr e)) IntT) = infer (TypeOf (E e) IntT)

-- -- (t-new)
infer (TypeOf exp@(E (NewExpr id)) nt@(NewTypeT id')) = do
  gi <- ask
  pure [[(pure $ id == id' && M.member nt (dt gi))]]

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
infer (TypeOf (S (AssignStmt (ID id) e)) UnitT) = do
    gi <- ask
    pure $ join $ [runReader (ands t) gi | t <- typeUniverse gi]
    where
        ands t = proveAND [TypeOf (E $ NameExpr $ ID id) t, Assignable (E e) t]
      
-- -- (t-block)
infer (TypeOf (S (BlockStmt s)) t) = infer (TypeOf (S s) t)

-- -- (t-seq)
infer (TypeOf (S (SeqStmt s1 s2)) t) = proveAND [TypeOf (S s1) UnitT, TypeOf (S s2) t]

-- -- (t-skip)
infer (TypeOf (S (SkipStmt)) UnitT) = true

-- -- (t-if)
infer (TypeOf (S (IfThenElseStmt e s1 s2)) t) = proveAND [TypeOf (E e) t, TypeOf (S s1) t, TypeOf (S s2) t]

-- -- (t-while)
infer (TypeOf (S (WhileStmt e s)) UnitT) = proveAND [TypeOf (E e) IntT, TypeOf (S s) UnitT]

-- -- (t-return)
infer (TypeOf (S (ReturnStmt e)) t) = infer (TypeOf (E e) t)

-- -- (t-stmtprog)
infer (TypeOf (P (StmtProg s)) t) = infer (TypeOf (S s) t)

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
infer (TypeOf (P (DeclProg (NewTypeDecl _ _) p)) t) = infer (TypeOf (P p) t)

-- -- (assignable-bottom)
infer exp@(Assignable _ TBOTTOM) = false

-- -- (assignable-null)
infer exp@(Assignable (E e) NullT) = do
  gi <- ask
  proveOR (M.keys (dt gi) >>= \tid -> [[TypeOf (E e) tid]])

-- -- (assignable-ty)
infer (Assignable exp@(E e) t) = infer (TypeOf (E e) t)

-- (t-fail)
infer _ = false 
