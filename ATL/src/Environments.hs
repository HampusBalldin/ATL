module Environments where

import Control.Monad.Reader
import Control.Monad.State
import Control.Monad.Except
import Control.Monad.Identity
import ATL

-- Possible Values
data V = ValV {val :: Val}
       | RefV {ref :: REF}
       | SubV {sub :: SUB}
       | SecretV {secret :: V} -- Add Secret Value
       | VBOTTOM
       deriving (Eq, Show)

-- Possible Types
data T = IntT
       | NullT 
       | UnitT 
       | SecretT T
       | FunctionTypeT [T] T
       | NewTypeT [T]
       deriving (Eq, Show)

addV :: V -> V -> V
addV (ValV (Number n1)) (ValV (Number n2)) = ValV (Number (n1 + n2))
addV (SecretV v1) v2 = SecretV (addV v1 v2)
addV v1 (SecretV v2) = SecretV (addV v1 v2)

-- Subprocess
data SUB = SUB Stmt [Identifier]
         | SUBBOTTOM
         deriving (Eq, Show)

-- Environment
type E         = Identifier -> V

-- Subprocess Environment
type Ep        = Identifier -> SUB
type D0        = Identifier -> Identifier -> V

-- HeapObject Enivronment
type HObj      = Identifier -> V

-- Heap Environment
-- Note that we need one extra level of indirection to model the heap!
type H         = REF -> HObj
type REF       = Name
type Eval      = (H, E, Either Stmt Expr) -> ExceptT String (ReaderT GlobalInfo (StateT Int IO)) (H, Either E V)

-- Gamma Type Environment
type GT          = Identifier -> T
type TypeEvalRes = ExceptT String (ReaderT (GlobalInfo) (StateT GT Identity)) T
type TypeEval    = Either Stmt Expr -> TypeEvalRes

-- Global Information for types and Functions
type GlobalInfo = (D0, Ep)
