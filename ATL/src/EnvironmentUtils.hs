module EnvironmentUtils where

import Data.Map (Map, member, (!?), adjust, insert, empty)

import Environments
import ATL

type Simple a b = a -> b
type Indirect a b c = a -> b -> c

singleSimple :: (Eq a) => b -> a -> b -> Simple a b
singleSimple b' a b a' = if a == a' then b else b'

singleIndirect :: (Eq a) => c -> a -> Simple b c -> Indirect a b c
singleIndirect c a sbc a' b = if a == a' then sbc b else c

extendSimple :: (Eq b) => b -> Simple a b -> Simple a b -> Simple a b
extendSimple b sold snew a = let r = snew a in if r /= b then r else sold a

extendIndirect :: (Eq c) => c -> Indirect a b c -> Indirect a b c -> Indirect a b c
extendIndirect c iold inew a b = let r = inew a b in if r /= c then r else iold a b
  
newE :: E
newE = const VBOTTOM

singleE :: Identifier -> V -> E
singleE = singleSimple VBOTTOM -- id v id' = if id == id' then v else VBOTTOM

extendE :: E -> E -> E
extendE = extendSimple VBOTTOM

newHObj :: HObj
newHObj = const VBOTTOM

singleHObj :: Identifier -> V -> HObj
singleHObj = singleSimple VBOTTOM 

extendHObj :: HObj -> HObj -> HObj
extendHObj = extendSimple VBOTTOM

newH :: H
newH = const newHObj

singleH :: REF -> HObj -> H
singleH = singleIndirect VBOTTOM

extendH :: H -> H -> H 
extendH = extendIndirect VBOTTOM

updateHAt :: H -> REF -> HObj -> H
updateHAt hold ref hobj ref' id = if ref == ref' then hobj id else hold ref' id

newEp :: Ep
newEp = const SUBBOTTOM

singleEp :: Identifier -> SUB -> Ep
singleEp = singleSimple SUBBOTTOM 

extendEp :: Ep -> Ep -> Ep
extendEp = extendSimple SUBBOTTOM 

newD0 :: D0
newD0 = const . const $ VBOTTOM

singleD0 :: Identifier -> E -> D0
singleD0 = singleIndirect VBOTTOM

extendD0 :: D0 -> D0 -> D0
extendD0 = extendIndirect VBOTTOM

newDT :: DT
newDT = empty

extendDT :: DT -> T -> Identifier -> T -> DT
extendDT dtold tid id t = ext (dtold !? tid)
  where
    ext Nothing   = insert tid (singleGT id t) dtold
    ext (Just gt) = adjust (flip extendGT (singleGT id t)) tid dtold


newGT :: GT
newGT = const TBOTTOM

singleGT :: Identifier -> T -> GT
singleGT = singleSimple TBOTTOM 

extendGT :: GT -> GT -> GT
extendGT = extendSimple TBOTTOM

bindGT :: GT -> TypeBinding -> GT
bindGT = foldl (\gt (id, t) -> extendGT gt (singleGT id (mapT t)))

newGlobalInfo :: GlobalInfo
newGlobalInfo = (newD0, newDT, newEp)

mapN :: Type -> V
mapN IntType = ValV $ Number 0
mapN  _      = ValV NULL

mapT :: Type -> T
mapT IntType     = IntT
mapT (IdType id) = NewTypeT id
