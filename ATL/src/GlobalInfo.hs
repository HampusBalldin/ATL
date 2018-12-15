module GlobalInfo where

import qualified Data.List as List
import Environments
import EnvironmentUtils
import ATL


evalGlobalInfo :: Decl -> GlobalInfo -> GlobalInfo
evalGlobalInfo (ProcDecl id tb s) (d0, dt, ep) = (d0, dt, ep')
    where
        ep' = extendEp ep (singleEp id (SUB s (fst <$> tb)))
        
evalGlobalInfo (NewTypeDecl id tb) (d0, dt, ep) = (extendD0 d0 d0', dt', ep)
    where
        d0' = List.foldl' (\dacc (idi, tyi) -> extendD0 dacc (singleD0 id (singleE idi (mapN tyi)))) newD0 tb
        -- TypeVar
        dt' = List.foldl' (\dacc (idi, tyi) -> extendDT dacc (NewTypeT id) idi (mapT tyi)) newDT tb

globalInfo :: Prog -> GlobalInfo
globalInfo (DeclProg d p) = evalGlobalInfo d (globalInfo p)
globalInfo (StmtProg _)   = newGlobalInfo

d0 :: GlobalInfo -> D0
d0 (d0, _, _) = d0

dt :: GlobalInfo -> DT
dt (_, dt, _) = dt

ep :: GlobalInfo -> Ep
ep (_, _, ep) = ep
