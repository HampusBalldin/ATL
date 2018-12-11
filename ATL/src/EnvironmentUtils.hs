module EnvironmentUtils where

import Environments
import ATL

newE :: E
newE = const VBOTTOM

singleE :: Identifier -> V -> E
singleE id v id' = if id == id' then v else VBOTTOM

extendE :: E -> E -> E
extendE eold enew id = let nid = enew id in if nid /= VBOTTOM then nid else eold id

newHObj :: HObj
newHObj = const VBOTTOM

singleHObj :: Identifier -> V -> HObj
singleHObj id v id' = if id == id' then v else VBOTTOM

extendHObj :: HObj -> HObj -> HObj
extendHObj hOold hOnew id = let nv = hOnew id in if nv /= VBOTTOM then nv else hOold id

newH :: H
newH = const newHObj

singleH :: REF -> HObj -> H
singleH r ho r' id = if r == r' then ho id else VBOTTOM

extendH :: H -> H -> H 
extendH hold hnew ref id = let hobj = hnew ref
                               nid  = hobj id
                           in if nid /= VBOTTOM then nid else hold ref id

updateHAt :: H -> REF -> HObj -> H
updateHAt hold ref hobj ref' id = if ref == ref' then hobj id else hold ref' id

globalEnv :: (GlobalInfo, Prog) -> GlobalInfo
globalEnv = undefined

newEp :: Ep
newEp = const SUBBOTTOM

singleEp :: Identifier -> SUB -> Ep
singleEp id sub id' = if id == id' then sub else SUBBOTTOM

extendEp :: Ep -> Ep -> Ep
extendEp epold epnew id = let sub = epnew id in if sub /= SUBBOTTOM then sub else epold id

newD0 :: D0
newD0 = const . const $ VBOTTOM

singleD0 :: Identifier -> E -> D0
singleD0 id d0e id1 id2 = if id == id1 then d0e id2 else ValV NULL

extendD0 :: D0 -> D0 -> D0
extendD0 d0old d0new id1 id2 = let d = d0new id1 id2 in if d /= VBOTTOM then d else d0old id1 id2 

newGlobalInfo :: GlobalInfo
newGlobalInfo = (newD0, newEp)

mapN :: Type -> V
mapN IntType = ValV $ Number 0
mapN  _      = ValV NULL
