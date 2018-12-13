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


infRules :: IR
infRules (Right e@(ValExpr (Number n))) = pure $ Infer [] (TypeOf e IntT)
infRules (Right e@(ValExpr NULL))       = pure $ Infer [] (TypeOf e NullT)
infRules (Right e@(NameExpr (ID id)))   = pure $ Infer [GammaConstr id (TypeVar "t")] (TypeOf e (TypeVar "t"))


testEq :: String -> T -> T -> TypeEvalRes
testEq err t1 t2 
    | t1 == t2  = pure t1
    | otherwise = throwError err

maybeFail :: String -> Maybe T -> TypeEvalRes
maybeFail err Nothing = throwError err
maybeFail _ (Just t)  = pure t

funcID :: Identifier -> Identifier
funcID = ("F_" ++)

typeEval :: TypeEval
-- (t-num)
typeEval (Right (ValExpr (Number n))) = pure IntT

-- (t-null)
typeEval (Right (ValExpr NULL)) = pure NullT

-- (t-id)
typeEval (Right (NameExpr (ID id))) = do
  gt <- get
  let res = gt id
  if res == TBOTTOM then throwError ("No type for: " ++ id)
  else pure res

-- (t-call)
typeEval (Right (CallExpr id es)) = do
  gt <- get
  case gt (funcID id) of
    FuncT at rt -> do
      tes <- sequence (typeEval . Right <$> es)
      let eqs = zip at tes
      if and ((uncurry (==)) <$> eqs) then pure rt else throwError ("Mismatching types at: " ++ show (CallExpr id es))
      
    _ -> throwError ("No func binding for: " ++ (funcID id))

-- (t-plus)
typeEval (Right (AddExpr e1 e2)) = do
   t1 <- typeEval (Right e1)
   t2 <- typeEval (Right e2)
   maybeFail ("Mismatch for types: " ++ show t1 ++ ", " ++ show t2 ++ " at " ++ show (AddExpr e1 e2)) (mappendTypes t1 t2)

-- (t-print)
typeEval (Right (PrintExpr e)) = do
    t <- typeEval (Right e)
    testEq ("Expected type INT at: " ++ show (PrintExpr e) ++ ", but got " ++ show t) t IntT
