{-# LANGUAGE BangPatterns, ScopedTypeVariables #-}
module CausetGenerator (
    randomBoxCauset
) where

import Data.Array
import Causet
import MatrixCauset
import FlatSpacetime

randomBoxCauset :: Int -> CausetInd -> IO MatrixCauset
randomBoxCauset d n = do
    points :: [FlatEvent] <- sequence $ replicate n $ randomBoxEvent d
    let ! pointsArr = array (0, n-1) $ zip [0..] points
    let ! relArr =
            array ((0, 0), (n-1, n-1))
                [((x, y), flatSpacetimeRel (pointsArr!x) (pointsArr!y))
                    | x <- [0..(n-1)], y <- [0..(n-1)]]
    return $ MatrixCauset n relArr
