-- Just a little experiment on how the implement an n-ary splittable unique
-- name supply.
module UniqueSupply where

import Control.Applicative
import Control.Monad.State

data Supply = Supply Integer Integer
  deriving( Eq, Ord, Show )

newtype Unique = Unique { getKey :: Integer }

instance Show Unique where
  show u = '#' : show (getKey u)

newSupply = Supply 0 1

getUniqueRaw :: Supply -> (Unique, Supply)
getUniqueRaw (Supply next step) = (Unique next, Supply (next + step) step)

splitSupply :: Integer -> Supply -> (Integer -> Supply)
splitSupply n (Supply next step) =
  \i -> if 0 <= i && i < n
          then Supply (next + i * step) (step * n)
          else moduleError "splitSupply: split-id out of range"

moduleError :: String -> a
moduleError msg = error $ "module UniqueSupply:" ++ msg


type UniqueM = State Supply

getUnique :: UniqueM Unique
getUnique = state getUniqueRaw

separate :: [UniqueM a] -> UniqueM [a]
separate ms = do
    supply <- get
    let nthSupply = splitSupply (fromIntegral (length ms) + 1) supply
    xs <- forM (zip [1..] ms) $ \(i, m) -> put (nthSupply i) >> m
    put $ nthSupply 0
    return xs

test = (`runState` newSupply) $ do
    separate $ replicate 2 $ separate $ replicate 2 $ separate $ replicate 2 $ uniques 2
    -- (,) <$> (separate $ replicate 3 $ uniques 8)
    --     <*> uniques 8
  where
    uniques n = sequence $ replicate n $ getUnique


