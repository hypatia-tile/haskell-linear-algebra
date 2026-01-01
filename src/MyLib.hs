module MyLib (someFunc) where

import Matrix

testMatrix2 :: IO ()
testMatrix2 = do
  let
    em =
      mkSquareMatrix
        4
        [ [1, 2, 4, 0]
        , [3, 4, 5, 6]
        , [5, 6, 0, 2]
        , [2, 0, 3, 5]
        ]
  eitherPrint em
  eitherPrint (inverse =<< em)
  eitherPrint $ do
    m <- em
    im <- inverse m
    mulSqMat m im

eitherPrint :: (Show a) => Either String a -> IO ()
eitherPrint (Left err) = putStrLn $ "Error: " <> err
eitherPrint (Right val) = putStrLn $ "Result: " <> show val

someFunc :: IO ()
someFunc = testMatrix2
