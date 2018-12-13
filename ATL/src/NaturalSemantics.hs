module NaturalSemantics where

import Control.Monad.Reader
import Control.Monad.State
import Control.Monad.Except
import Control.Monad

import Data.Either
import qualified Data.List as List
import Environments
import EnvironmentUtils
import ATL
import Debug.Trace

valv = Right . ValV
refv = Right . RefV 
num  = valv . Number
zeronum = num 0
procEnvtoEnv :: Ep -> E
procEnvtoEnv ep id = SubV (ep id)

eProc :: GlobalInfo -> Ep
eProc (_, ep) = ep

isNum :: V -> Bool
isNum (ValV (Number _)) = True
isNum _                 = False

fresh :: Int -> REF
fresh index = ID ("REF_" ++ show index)

runEvalHelper :: GlobalInfo -> Prog -> IO (Either String V)
runEvalHelper gi (DeclProg d p) = do
  let gi' = evalGlobalInfo d gi
  runEvalHelper gi' p
  
runEvalHelper gi (StmtProg s) = do
  res <- evalStateT (runReaderT (runExceptT (eval (newH, procEnvtoEnv (eProc gi), Left s))) gi) 0
  case res of
    Left err -> return $ Left err
    Right (_, Right v) -> return $ Right v
    _                  -> return $ Left $ "Got Env back"

runEval :: Prog -> IO (Either String V)
runEval = runEvalHelper newGlobalInfo

evalGlobalInfo :: Decl -> GlobalInfo -> GlobalInfo
evalGlobalInfo (ProcDecl id tb s) (d0, ep) = (d0, ep')
    where
        ep' = extendEp ep (singleEp id (SUB s (fst <$> tb)))
        
evalGlobalInfo (NewTypeDecl id tb) (d0, ep) = (extendD0 d0 d0', ep)
    where
        d0' = List.foldl' (\dacc (idi, tyi) -> extendD0 dacc (singleD0 id (singleE idi (mapN tyi)))) newD0 tb

eval :: Eval

-- (val)
eval (h, _, Right (ValExpr v)) = pure (h, valv v)

-- (id)
eval (h, env, Right (NameExpr (ID id))) = pure (h, Right (env id))

-- (plus)
eval (h, env, Right (AddExpr e1 e2)) = do
  (h', Right v1)  <- eval (h, env, Right e1)
  (h'', Right v2) <- eval (h', env, Right e2)
  pure (h'', Right (addV v1 v2))
  
-- (call)
eval (h0, env, Right (CallExpr id args)) = do
  let SubV (SUB s fargs) = env id
  (hk, eLoc) <- foldM evalArg (h0, newE) (zip fargs args)
  (_, ep) <- ask
  eval (hk, extendE (procEnvtoEnv ep) eLoc, Left s)
  where
    evalArg (hi, eLoc) (farg, ei) = do
      (h, Right vi) <- eval (hi, env, Right ei)
      pure (h, extendE eLoc (singleE farg vi))

-- (print)
eval (h, env, Right (PrintExpr e)) = do
  (h', Right v) <- eval (h, env, Right e)
  case v of
    SecretV v' -> throwError "Attempt to Print a Secret"
    _          -> do
                    liftIO $ putStrLn ("PRINT: " ++ show v)
                    pure (h', zeronum)

-- (new)
eval (h, env, Right (NewExpr id)) = do
  (d0, _) <- ask
  index   <- get
  let ref = fresh index
  pure (extendH h (singleH ref (d0 id)), refv ref)

-- (load)
eval (h, env, Right (NameExpr (DeRef base field))) = do
  (h', Right (RefV r)) <- eval (h, env, Right $ NameExpr base)
  pure (h', Right (h' r field))

-- (store)
eval (h, env, Left (AssignStmt (DeRef base field) lhs)) = do
  (h', Right (RefV r)) <- eval (h, env, Right $ NameExpr base)
  (h'', Right v) <- eval (h', env, Right lhs)
  pure (updateHAt h'' r (extendHObj (h'' r) (singleHObj field v)), Left env)

-- (assign)
eval (h, env, Left (AssignStmt (ID id) e)) = do
  (h', Right v) <- eval (h, env, Right e)
  pure (h', Left $ extendE env (singleE id v))

-- (block)
eval (h,env, Left (BlockStmt s)) = eval (h, env, Left s)

-- (seq)
eval (h, env, Left (SeqStmt s1 s2)) = do
  (h', Left env') <- eval(h, env, Left s1)
  eval(h', env', Left s2)

-- (skip)
eval (h, env, Left SkipStmt) = pure (h, Left env)

-- (if-then-else)
eval (h, env, Left (IfThenElseStmt e s1 s2)) = do
  (h', Right v) <- eval (h, env, Right e)
  if isNum v && integer (val v) /= 0 then
    eval (h', env, Left s1)
  else 
    eval (h', env, Left s2)

-- (while)
eval (h, env, Left (WhileStmt e s)) = eval (h, env, Left (IfThenElseStmt e (SeqStmt s (WhileStmt e s)) SkipStmt))

-- (return)
eval (h, env, Left (ReturnStmt e)) = do
  (h', Right v) <- eval (h, env, Right e)
  pure (h', Right v)

-- (secret)
eval (h, env, Right (SecretExpr e)) = do
  (h', Right v) <- eval (h, env, Right e)
  pure (h', Right (SecretV v))
