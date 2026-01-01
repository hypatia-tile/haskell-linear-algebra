module Vector (
  mkVec,
  dim,
  dot,
  add,
  scale,
  norm2,
  mkMatrix,
  linMap,
) where

import Control.Monad (forM, (>=>))
import LA.Linear

data Vec = Vec
  { dim_ :: Int
  , coords :: [Double]
  }
  deriving (Show, Eq)

data Matrix = Matrix
  { nrows :: Int
  , ncols :: Int
  , entries :: [[Double]]
  }
  deriving (Show, Eq)

instance VectorSpace Vec where
  vadd = add
  vscale = scale

-- A vector is just a list of numbers (We'll be strict later).
mkVec :: Int -> [Double] -> Either String Vec
mkVec n xs
  | n < 0 = Left "mkVec: negative length"
  | len /= n = Left $ "mkVec: expected length " <> show n <> ", got " <> show len
  | otherwise = Right (Vec n xs)
 where
  len = length xs

dim :: Vec -> Int
dim = dim_

sameDim :: Vec -> Vec -> Bool
sameDim v1 v2 = dim v1 == dim v2

dot :: Vec -> Vec -> Either String Double
dot v1 v2
  | sameDim v1 v2 = Right . sum $ zipWith (*) (coords v1) (coords v2)
  | otherwise = Left "dot: dimension mismatch"

add :: Vec -> Vec -> Either String Vec
add v1 v2
  | sameDim v1 v2 = Right v1{coords = zipWith (+) (coords v1) (coords v2)}
  | otherwise = Left "add: dimension mismatch"

scale :: Double -> Vec -> Vec
scale a v = v{coords = map (a *) (coords v)}

norm2 :: Vec -> Either String Double
norm2 v = dot v v

transpose :: Matrix -> Matrix
transpose m =
  Matrix
    { nrows = ncols m
    , ncols = nrows m
    , entries = [[entries m !! i !! j | i <- [0 .. nrows m - 1]] | j <- [0 .. ncols m - 1]]
    }

type ColVecs = [[Double]]

mkMatrix :: Int -> Int -> ColVecs -> Either String Matrix
mkMatrix r c colVecs =
  if length colVecs /= c
    then Left $ "mkMatrix: expected " <> show c <> " columns, got " <> show (length colVecs)
    else do
      cols <- forM colVecs $ \col -> mkVec r col
      Right . transpose $
        Matrix
          { nrows = c
          , ncols = r
          , entries = map coords cols
          }

linMap :: Matrix -> LinearE Vec Vec
linMap m = Linear $ \v ->
  let dimV = dim v
   in forM (entries m) (mkVec dimV >=> dot v) >>= mkVec (nrows m)
