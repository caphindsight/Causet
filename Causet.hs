module Causet (
    CauseRel(..),
    CausetInd,
    Causet, causetSize, causetRel
) where

data CauseRel =
    Precedes | Follows | NotRelated | Equals
    deriving (Show, Read, Eq)

type CausetInd = Int

class Causet c where
    causetSize :: c -> CausetInd
    causetRel  :: c -> CausetInd -> CausetInd -> CauseRel


