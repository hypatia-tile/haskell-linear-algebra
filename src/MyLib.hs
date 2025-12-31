module MyLib (someFunc) where

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

binE :: (a -> b -> Either e c) -> Either e a -> Either e b -> Either e c
binE f ea eb = do
  a <- ea
  b <- eb
  f a b

add :: Vec -> Vec -> Either String Vec
add v1 v2
  | dim v1 /= dim v2 = Left "add: dimension mismatch"
  | otherwise = Right . Vec $ zipWith (+) (coords v1) (coords v2)

scale :: Double -> Vec -> Vec
scale a v = Vec $ map (a *) (coords v)

norm2 :: Vec -> Either String Double
norm2 v = dot v v

someFunc :: IO ()
someFunc = do
  let
    v = mkVec 3 [1, 2, 3]
    w = mkVec 3 [4, 5, 6]
    u = mkVec 2 [5, 6]
    x = mkVec 2 [4, 5, 6]
  putStrLn $ "x . w = " <> show (binE dot x w)
  putStrLn $ "v . w = " <> show (binE dot v w)
  putStrLn $ "u . w = " <> show (binE dot u w)
  putStrLn $ "v + w = " <> show (binE add v w)
  putStrLn $ "u + w = " <> show (binE add u w)
  putStrLn $ "2 * v = " <> show (scale 2 <$> v)
  putStrLn $ "||v||^2 = " <> show (norm2 <$> v)
