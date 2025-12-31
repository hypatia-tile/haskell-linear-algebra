module MyLib (someFunc) where

import Vector

binE :: (a -> b -> Either e c) -> Either e a -> Either e b -> Either e c
binE f ea eb = do
  a <- ea
  b <- eb
  f a b

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
