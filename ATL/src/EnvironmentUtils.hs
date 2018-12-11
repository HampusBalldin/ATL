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
