module Types where

import Control.Monad.Reader
import Control.Monad.Except
import Control.Monad

import ATL
import Environments
import EnvironmentUtils

typeval :: TypeEval
-- (t-num)
typeval (gt, Right (ValExpr (Number n))) = pure IntT

-- (t-null)
typeval (gt, Right (ValExpr NULL)) = pure NullT

-- (t-id)
typeval (gt, Right (NameExpr (ID id))) = pure $ gt id
