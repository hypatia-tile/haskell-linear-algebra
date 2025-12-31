module MyLib (someFunc) where

-- A vector is just a list of numbers (We'll be strict later).
type Vec = [Double]

dot :: Vec -> Vec -> Double
dot [] [] = 0
dot (x : xs) (y : ys) = x * y + dot xs ys
dot _ _ = error "dot: length mismatch"

add :: Vec -> Vec -> Vec
add [] [] = []
add (x : xs) (y : ys) = (x + y) : add xs ys
add _ _ = error "add: length mismatch"

scale :: Double -> Vec -> Vec
scale _ [] = []
scale s (x : xs) = s * x : scale s xs

norm2 :: Vec -> Double
norm2 v = dot v v

someFunc :: IO ()
someFunc = do
  let
    v = [1, 2, 3]
    w = [4, 5, 6]
  putStrLn $ "v . w = " <> show (dot v w)
  putStrLn $ "v + w" <> show (add v w)
  putStrLn $ "2 * v = " <> show (scale 2 v)
  putStrLn $ "||v||^2 = " <> show (norm2 v)
