module Matrix where

import Control.Monad (foldM, forM, forM_, (>=>))
import LA.Linear
import Vector (Vec, dim, dot, mkVec)

data Matrix = Matrix
  { nrows :: Int
  , ncols :: Int
  , entries :: [[Double]]
  }
  deriving (Show, Eq)

data SquareMatrix = SQuareMatrix
  { size :: Int
  , sEntries :: [[Double]]
  }
  deriving (Show, Eq)

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
      forM_ colVecs $ \col -> mkVec r col
      Right . transpose $
        Matrix
          { nrows = c
          , ncols = r
          , entries = colVecs
          }

mkSquareMatrix :: Int -> ColVecs -> Either String SquareMatrix
mkSquareMatrix n colVecs = do
  m <- mkMatrix n n colVecs
  Right $ SQuareMatrix n (entries m)

cofactor :: SquareMatrix -> Int -> Int -> Either String Double
cofactor sm i j
  | i < 0 || i >= sizem = Left "submatrix: row index out of bounds"
  | j < 0 || j >= sizem = Left "submatrix: column index out of bounds"
  | otherwise = determinant $ SQuareMatrix (size sm - 1) newEntries
 where
  sizem = size sm
  removeN n = (\(pre, post) -> pre <> tail post) . splitAt n
  newEntries =
    map (removeN j) (removeN i (sEntries sm))

-- Unsafe calculation of determinant
determinant :: SquareMatrix -> Either String Double
determinant sm
  | n < 1 = Left "determinant: matrix size must be at least 1"
  | n == 1 = case sEntries sm of
      [[x]] -> Right x
      _ -> Left "determinant: invalid square matrix"
  | otherwise = foldM addFactor 0 [0 .. n - 1]
 where
  n = size sm
  addFactor :: Double -> Int -> Either String Double
  addFactor acc i =
    cofactor sm i 0 >>= \cof ->
      let
        sign = (-1) ^ i
        val = head (sEntries sm !! i)
       in
        Right $ acc + sign * val * cof

scaleMat :: Double -> Matrix -> Matrix
scaleMat a m = m{entries = map (map (a *)) (entries m)}

addMat :: Matrix -> Matrix -> Either String Matrix
addMat m1 m2
  | nrows m1 /= nrows m2 || ncols m1 /= ncols m2 =
      Left "addMat: dimension mismatch"
  | otherwise =
      Right $
        Matrix
          { nrows = nrows m1
          , ncols = ncols m1
          , entries = zipWith (zipWith (+)) (entries m1) (entries m2)
          }

mulMat :: Matrix -> Matrix -> Either String Matrix
mulMat m1 m2 = do
  let
    r = nrows m1
    c = ncols m2
    rows1 = entries m1
    cols2 = entries . transpose $ m2
  entry <- forM rows1 $ \row1 -> do
    rowVec1 <- mkVec r row1
    forM cols2 $ \col2 -> do
      colVec2 <- mkVec c col2
      dot rowVec1 colVec2
  mkMatrix r c entry

scaleSqMat :: Double -> SquareMatrix -> SquareMatrix
scaleSqMat a sm = SQuareMatrix (size sm) (map (map (a *)) (sEntries sm))

addSqMat :: SquareMatrix -> SquareMatrix -> Either String SquareMatrix
addSqMat sm1 sm2 = do
  let
    m1 = Matrix (size sm1) (size sm1) (sEntries sm1)
    m2 = Matrix (size sm2) (size sm2) (sEntries sm2)
  mRes <- addMat m1 m2
  Right $ SQuareMatrix (nrows mRes) (entries mRes)

mulSqMat :: SquareMatrix -> SquareMatrix -> Either String SquareMatrix
mulSqMat sm1 sm2 = do
  let
    m1 = Matrix (size sm1) (size sm1) (sEntries sm1)
    m2 = Matrix (size sm2) (size sm2) (sEntries sm2)
  mRes <- mulMat m1 m2
  Right $ SQuareMatrix (nrows mRes) (entries mRes)

inverse :: SquareMatrix -> Either String SquareMatrix
inverse sm = case determinant sm of
  Left err -> Left err
  Right det
    | det == 0 -> Left "inverse: matrix is singular"
    | otherwise -> case cofactorMatrix of
        Left err -> Left err
        Right cm ->
          Right $
            scaleSqMat
              (1 / det)
              sm
                { sEntries = cm
                }
   where
    cofactorMatrix :: Either String [[Double]]
    cofactorMatrix = do
      let n = size sm
      forM [0 .. n - 1] $ \i -> do
        forM [0 .. n - 1] $ \j -> do
          cof <- cofactor sm j i
          let sign = (-1) ^ (j + i)
          Right $ sign * cof

linMap :: Matrix -> LinearE Vec Vec
linMap m = Linear $ \v ->
  let dimV = dim v
   in forM (entries m) (mkVec dimV >=> dot v) >>= mkVec (nrows m)
