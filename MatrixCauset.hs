module MatrixCauset (
    MatrixCauset(..)
) where

import Data.Array
import Causet

data MatrixCauset =
    MatrixCauset CausetInd (Array (CausetInd, CausetInd) CauseRel)

instance Causet MatrixCauset where
    causetSize (MatrixCauset n _) = n
    causetRel  (MatrixCauset n a) x y
      | x < 0 || x >= n
            = errorCausetIndexOutOfBound x (0, n - 1)
      | y < 0 || y >= n
            = errorCausetIndexOutOfBound y (0, n - 1)
      | otherwise
            = a ! (x, y)
      where
        errorCausetIndexOutOfBound i (a, b) =
            error $ "Causet index " ++ show i ++ " is out of bound " ++ show (a, b) ++ "!"
