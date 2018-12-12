module Types where

import Control.Monad.Reader
import Control.Monad.Except
import Control.Monad.Identity
import Control.Monad.State
import Control.Monad

import ATL
import Environments
import EnvironmentUtils

mappendTypes :: T -> T -> Maybe T
mappendTypes (SecretT t1) t2 = SecretT <$> mappendTypes t1 t2
mappendTypes t1 (SecretT t2) = SecretT <$> mappendTypes t1 t2
mappendTypes t1 t2           = if t1 == t2 then Just t1 else Nothing

testEq :: String -> T -> T -> TypeEvalRes
testEq err t1 t2 
    | t1 == t2  = pure t1
    | otherwise = throwError err

maybeFail :: String -> Maybe T -> TypeEvalRes
maybeFail err Nothing = throwError err
maybeFail _ (Just t)  = pure t

typeEval :: TypeEval
-- (t-num)
typeEval (Right (ValExpr (Number n))) = pure IntT

-- (t-null)
typeEval (Right (ValExpr NULL)) = pure NullT

-- (t-id)
typeEval (Right (NameExpr (ID id))) = ($id) <$> get

-- (t-plus)
typeEval (Right (AddExpr e1 e2)) = do
   t1 <- typeEval (Right e1)
   t2 <- typeEval (Right e2)
   maybeFail ("Mismatch for types: " ++ show t1 ++ ", " ++ show t2 ++ " at " ++ show (AddExpr e1 e2)) (mappendTypes t1 t2)

-- (t-print)
typeEval (Right (PrintExpr e)) = do
    t <- typeEval (Right e)
    testEq ("Expected type INT at: " ++ show (PrintExpr e) ++ ", but got " ++ show t) t IntT
