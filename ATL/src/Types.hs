module Types where

import Control.Monad.Reader
import Control.Monad.Except
import Control.Monad.Identity
import Control.Monad.State
import Control.Monad

import qualified Data.Map as M
import Data.Map (Map)
import qualified Data.List as L

import ATL
import GlobalInfo
import Environments
import EnvironmentUtils

import Debug.Trace

---------------------------------
-- Brute Force Type Inference
---------------------------------

mappendTypes :: T -> T -> T
mappendTypes (SecretT t1) t2 = SecretT $ mappendTypes t1 t2
mappendTypes t1 (SecretT t2) = SecretT $ mappendTypes t1 t2
mappendTypes t1 t2           = if t1 == t2 then t1 else TBOTTOM

printLine l = liftIO  $ putStrLn l

data Inferrable = E Expr
                | S Stmt
                | P Prog
               deriving (Eq, Show)

data InferType = TypeOf Inferrable T
               | Assignable Inferrable T
               | LocalStateIT (GT -> GT) InferType

data Inf = Leaf (State GT Bool)--(State GT Bool)
         | And [Inf]
         | Or [Inf]
         | LocalState (GT -> GT) Inf

type InferenceRes = Reader GlobalInfo Inf
type Inference = InferType -> InferenceRes

---------------------------------
-- Helper Functions
---------------------------------
localState :: (GT -> GT) -> [[State GT Bool]] -> [[State GT Bool]]
localState f st = fmap (locSingle f <$>) st
    where
      locSingle f st = get >>= \s -> put (f s) >> st >>= \a -> put s >> pure a

typeUniverse :: GlobalInfo -> [T]
typeUniverse gi = [NullT, IntT] ++ (M.keys (dt gi)) 

when2 :: Monad m => Bool -> a -> m a -> m a
when2 c d ma = if c then ma else pure d

proveAND :: [InferType] -> InferenceRes
proveAND infs = And <$> sequence (infer <$> infs)

proveOR :: [[InferType]] -> InferenceRes
proveOR infs = Or <$> sequence (proveAND <$> infs)

-- Reduce Tree to a sequence of state programs
-- Each inner list represents a proof path that keeps the Gamma Type Function GT as state.
allPaths :: Inf -> [[State GT Bool]]
allPaths (Leaf s)           = [[s]]
-- And gives Cartesian Product of Children
allPaths (And [])           = [[]]
allPaths (And (inf:infs))   = (++) <$> allPaths inf <*> allPaths (And infs)
-- Or gives Union of children
allPaths (Or infs)          = join $ allPaths <$> infs
-- Extend state for the given paths, when done with the paths restore state
allPaths (LocalState f inf) = localState f (allPaths inf)

-- Chain the type program to find a path through the proof-tree that gives a correct Gamma.
typeCheck :: Prog -> [Bool]
typeCheck p = findSol states
  where
    states = runReader (infer (TypeOf (P p) IntT)) gi
    gi = globalInfo p
    
findSol :: Inf -> [Bool]
findSol states = do
  andBlock <- allPaths states
  [runIdentity $ evalStateT (chainAND andBlock) newGT]

chainAND :: [State GT Bool] -> State GT Bool
chainAND []      = pure True
chainAND (s:sts) = do
    gt <- get
    let (b, gt') = runState s gt
    when2 b b $ do
        put gt'
        b' <- chainAND sts
        pure (b && b')

true, false :: InferenceRes
true  = pure $ Leaf (pure True)
false = pure $ Leaf (pure False)

allTypeAssignments :: GlobalInfo -> Int -> [[T]]
allTypeAssignments gi n = combinations base
  where
    base = replicate n (typeUniverse gi)
    combinations []       = [[]]
    combinations (ts:tss) = (:) <$> ts <*> combinations tss

---------------------------------
-- Build Proof Tree.
---------------------------------
infer :: Inference
-- (t-num)
infer (TypeOf (E (ValExpr (Number n))) IntT) = true

-- (t-null)
infer (TypeOf (E (ValExpr NULL)) NullT) = true 

-- (t-id)
infer (TypeOf (E (NameExpr (ID id))) t) = pure $ Leaf $ do
    gt <- get
    case gt id of
        TBOTTOM -> const True <$> put (extendGT gt (singleGT id t))
        t'      -> pure (t == t')

-- (t-call)
infer (TypeOf (E (CallExpr id args)) t0) = do
  gi <- ask
  proveOR [((\(e, t) -> TypeOf (E e) t) <$> tb) ++ [TypeOf (E $ NameExpr (ID id)) (FuncT (snd <$> tb) t0)] | tb <- zip args <$> (allTypeAssignments gi (length args))]

-- (t-plus)
infer (TypeOf (E (AddExpr e1 e2)) IntT) = proveAND [TypeOf (E e1) IntT, TypeOf (E e2) IntT]

-- (t-print)
infer (TypeOf (E (PrintExpr e)) IntT) = infer (TypeOf (E e) IntT)

-- (t-new)
infer (TypeOf (E (NewExpr id)) nt@(NewTypeT id')) = do
  gi <- ask
  pure $ Leaf $ pure $ id == id' && M.member nt (dt gi)

-- (t-load)
infer (TypeOf (E (NameExpr (DeRef e id))) t) = do
  gi <- ask
  proveOR [[TypeOf (E (NameExpr e)) tid] | tid <- M.keys (dt gi), t == (((dt gi) M.! tid) id)]

-- (t-store)
infer (TypeOf (S (AssignStmt (DeRef e id) lhs)) UnitT) = do
  gi <- ask
  proveOR (M.keys (dt gi) >>= \tid -> [inf gi tid])
  where
    inf gi tid = let t = ((dt gi) M.! tid) id in
                 [TypeOf (E (NameExpr e)) tid, Assignable (E lhs) t]

-- (t-assign)
infer (TypeOf (S (AssignStmt (ID id) e)) UnitT) = do
  gi <- ask
  proveOR [[TypeOf (E $ NameExpr $ ID id) t, Assignable (E e) t] | t <- typeUniverse gi]
      
-- (t-block)
infer (TypeOf (S (BlockStmt s)) t) = infer (TypeOf (S s) t)

-- (t-seq)
infer (TypeOf (S (SeqStmt s1 s2)) t) = proveAND [TypeOf (S s1) UnitT, TypeOf (S s2) t]

-- (t-skip)
infer (TypeOf (S (SkipStmt)) UnitT) = true

-- (t-if)
infer (TypeOf (S (IfThenElseStmt e s1 s2)) t) = proveAND [TypeOf (E e) t, TypeOf (S s1) t, TypeOf (S s2) t]

-- (t-while)
infer (TypeOf (S (WhileStmt e s)) UnitT) = proveAND [TypeOf (E e) IntT, TypeOf (S s) UnitT]

-- (t-return)
infer (TypeOf (S (ReturnStmt e)) t) = infer (TypeOf (E e) t)

-- (t-stmtprog)
infer (TypeOf (P (StmtProg s)) t) = infer (TypeOf (S s) t)

-- (t-proc)
infer (TypeOf (P (DeclProg (ProcDecl id tb s) p)) t) = do
  gi <- ask
  proveOR [declProof gi t0 | t0 <- typeUniverse gi]
  where
    declProof gi t0 = [TypeOf (E (NameExpr (ID id))) (FuncT (mapT . snd <$> tb) t0)
                      ,LocalStateIT (flip bindGT tb) (TypeOf (S s) t0)
                      ,TypeOf (P p) t]

-- (t-dataty)
infer (TypeOf (P (DeclProg (NewTypeDecl _ _) p)) t) = infer (TypeOf (P p) t)

-- (assignable-bottom)
infer (Assignable _ TBOTTOM) = false

-- (assignable-null)
infer (Assignable (E e) NullT) = do
  gi <- ask
  proveOR (M.keys (dt gi) >>= \tid -> [[TypeOf (E e) tid]])

-- (assignable-ty)
infer (Assignable (E e) t) = infer (TypeOf (E e) t)

-- (Helper Function for LocalState)
infer (LocalStateIT f (TypeOf s t)) = do
  gi <- ask
  pure $ LocalState f $ (runReader (infer (TypeOf s t)) gi)

-- -- (t-fail)
infer _ = false 
