module Environments where

import Control.Monad.Reader
import Control.Monad.State

import ATL

data V = ValV {val :: Val}
       | RefV {ref :: REF}
       | SubV {sub :: SUB}
       | VBOTTOM
       deriving (Eq, Show)

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
type Eval      = (H, E, Either Stmt Expr) -> ReaderT GlobalInfo (StateT Int IO) (H, Either E V)


type GlobalInfo = (Ep, D0)

newE :: E
newE = const VBOTTOM

newHObj :: HObj
newHObj = const VBOTTOM

newH :: H
newH = const newHObj 

extendE :: E -> E -> E
extendE eold enew id = let nid = enew id in if nid /= VBOTTOM then nid else eold id

extendH :: H -> H -> H 
extendH hold hnew ref id = let hobj = hnew ref
                               nid  = hobj id
                           in if nid /= VBOTTOM then nid else hold ref id

globalEnv :: (GlobalInfo, Prog) -> GlobalInfo
globalEnv = undefined

-- type EvalIO = Read
