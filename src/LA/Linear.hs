module LA.Linear (VectorSpace (..), Linear (Linear), LinearE, applyLinearEMap) where

-- Abstract interface for a vector space
class VectorSpace v where
  vadd :: v -> v -> Either String v
  vscale :: Double -> v -> v

newtype Linear m v w = Linear {runLinear :: v -> m w}

type LinearE = Linear (Either String)

applyLinearEMap :: (Monad m) => (VectorSpace v, VectorSpace w) => Linear m v w -> v -> m w
applyLinearEMap = runLinear
