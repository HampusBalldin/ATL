module NaturalSemantics where

import Control.Monad.Reader
import Control.Monad.State
import Control.Monad

import Data.Either
import qualified Data.List as List
import Environments
import EnvironmentUtils
import ATL

valv = Right . ValV
refv = Right . RefV 
num  = valv . Number
procEnvtoEnv :: Ep -> E
procEnvtoEnv ep id = SubV (ep id)

fresh :: Int -> REF
fresh index = ID ("REF_" ++ show index)

eval :: Eval

-- (val)
eval (h, _, Right (ValExpr v)) = pure (h, valv v)

-- (id)
eval (h, env, Right (NameExpr (ID id))) = pure (h, Right (env id))

-- (plus)
eval (h, env, Right (AddExpr e1 e2)) = do
  (h', Right (ValV (Number n1)))  <- eval (h, env, Right e1)
  (h'', Right (ValV (Number n2))) <- eval (h', env, Right e2)
  pure (h'', num (n1 + n2))
  
-- (call)
eval (h0, env, Right (CallExpr id args)) = do
  let SubV (SUB s fargs) = env id
  (hk, eLoc) <- foldM evalArg (h0, newE) (zip fargs args)
  (ep, _) <- ask
  eval (hk, extendE (procEnvtoEnv ep) eLoc, Left s)
  where
    evalArg (hi, eLoc) (farg, ei) = do
      (h, Right vi) <- eval (hi, env, Right ei)
      pure (h, extendE eLoc (singleE farg vi))

-- (print)
eval (h, env, Right (PrintExpr e)) = do
  (h', Right n) <- eval (h, env, Right e)
  liftIO $ putStrLn ("PRINT: " ++ show n)
  pure (h', valv NULL)

-- (new)
eval (h, env, Right (NewExpr id)) = do
  (_, d0) <- ask
  index   <- lift get
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

  
