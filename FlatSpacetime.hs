{-# LANGUAGE ScopedTypeVariables #-}
module FlatSpacetime (
    FlatEvent,
    intervalSquared,
    flatSpacetimeRel,
    randomBoxEvent
) where

import System.Random
import Causet

data FlatEvent =
    FlatEvent [Double]
    deriving (Show, Read, Eq)

intervalSquared :: FlatEvent -> Double
intervalSquared (FlatEvent (t:x)) = t^2 - sum [i^2 | i <- x]

flatSpacetimeRel :: FlatEvent -> FlatEvent -> CauseRel
flatSpacetimeRel (FlatEvent e1@(t1:x1)) (FlatEvent e2@(t2:x2))
  | e1 == e2
        = Equals
  | s < 0
        = NotRelated
  | t1 < t2
        = Precedes
  | t1 > t2
        = Follows
  | otherwise
        = error $ "Unexpected flat space causal relation between "
            ++ show e1 ++ " and " ++ show e2 ++ "!"
  where
    s = intervalSquared $ FlatEvent $ zipWith (-) e1 e2

randomBoxEvent :: Int -> IO FlatEvent
randomBoxEvent d = do
    arr :: [Double] <- sequence $ replicate d $ randomIO
    return $ FlatEvent arr
