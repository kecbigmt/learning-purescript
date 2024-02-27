module Calc where

import Prelude

import Data.Array (null, filter, concatMap, (..))
import Data.Array.Partial (tail, head)
import Partial.Unsafe (unsafePartial)
import Data.Foldable (product, foldl)
import Control.Alternative (guard)

myLength :: forall a. Array a -> Int
myLength arr = 
  if null arr
  then 0
  else 1 + myLength (unsafePartial tail arr)

isEven :: Int -> Boolean
isEven 0 = true
isEven 1 = false
isEven n = isEven (n - 2)

countEven :: Array Int -> Int
countEven arr = 
  if null arr
  then 0
  else (if isEven $ (unsafePartial head arr) then 1 else 0) + countEven (unsafePartial tail arr)

sqrtArray :: Array Int -> Array Int
sqrtArray arr = (\n -> n * n) <$> arr

infix 0 filter as <$?>

excludeNagative :: Array Int -> Array Int
excludeNagative arr = (\n -> n >= 0) <$?> arr

pairs :: Int -> Array (Array Int)
pairs n = concatMap (\i -> (\j -> [i, j]) <$> (i .. n)) (1 .. n)

factors :: Int -> Array (Array Int)
factors n = do
  i <- 1 .. n
  j <- i .. n
  guard $ i * j == n
  pure [i, j]

-- 素数かどうかを調べる関数
isPrime :: Int -> Boolean
isPrime n = (myLength $ factors n) == 1

count :: forall a. (a -> Boolean) -> Array a -> Int -> Int
count _ [] acc = acc
count p xs acc = if p (unsafePartial head xs)
                then count p (unsafePartial tail xs) (acc + 1)
                else count p (unsafePartial tail xs) acc

myReverse :: forall a. Array a -> Array a
myReverse arr = foldl (\acc x -> [x] <> acc) [] arr