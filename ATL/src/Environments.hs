module Environments where

import Control.Monad.Reader
import Control.Monad.State
import Control.Monad.Except
import Control.Monad.Identity
import qualified Data.Map as M
import Data.Map (Map)
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
       | NewTypeT Identifier
       | FuncT [T] T
       | TBOTTOM
       deriving (Eq, Show, Ord)

-- Subprocess
data SUB = SUB Stmt [Identifier]
         | SUBBOTTOM
         deriving (Eq, Show)

-- Environment
type E         = Identifier -> V

-- Subprocess Environment
type Ep        = Identifier -> SUB
type D0        = Identifier -> Identifier -> V
type DT        = Map T (Identifier -> T)

-- HeapObject Enivronment
type HObj      = Identifier -> V

-- Heap Environment
-- Note that we need one extra level of indirection to model the heap!
type H         = REF -> HObj
type REF       = Name
type Eval      = (H, E, Either Stmt Expr) -> ExceptT String (ReaderT GlobalInfo (StateT Int IO)) (H, Either E V)

-- Gamma Type Environment
type GT          = Identifier -> T

-- Global Information for types and Functions
type GlobalInfo = (D0, DT, Ep)
