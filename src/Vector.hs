module Vector (
  mkVec,
  dim,
  dot,
  add,
  scale,
  norm2,
) where

-- A vector is just a list of numbers (We'll be strict later).
newtype Vec = Vec {coords :: [Double]} deriving (Show, Eq)

mkVec :: Int -> [Double] -> Either String Vec
mkVec n xs
  | n < 0 = Left "mkVec: negative length"
  | len /= n = Left $ "mkVec: expected length " <> show n <> ", got " <> show len
  | otherwise = Right (Vec xs)
 where
  len = length xs

dim :: Vec -> Int
dim = length . coords

dot :: Vec -> Vec -> Either String Double
dot v1 v2
  | dim v1 /= dim v2 = Left "dot: dimension mismatch"
  | otherwise = Right . sum $ zipWith (*) (coords v1) (coords v2)

add :: Vec -> Vec -> Either String Vec
add v1 v2
  | dim v1 /= dim v2 = Left "add: dimension mismatch"
  | otherwise = Right . Vec $ zipWith (+) (coords v1) (coords v2)

scale :: Double -> Vec -> Vec
scale a v = Vec $ map (a *) (coords v)

norm2 :: Vec -> Either String Double
norm2 v = dot v v
