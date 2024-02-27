module FileOperations where

import Prelude
import Data.Maybe
import Data.Path
import Data.Array ((:), concatMap, filter, head)
import Data.Foldable (foldl)
import Control.Alternative (guard)

allFiles :: Path -> Array Path
allFiles file = file : concatMap allFiles (ls file)

allFiles' :: Path -> Array Path
allFiles' file = file : do
  child <- ls file
  allFiles' child

-- （簡単）ディレクトリの全てのサブディレクトリの中にある
-- （ディレクトリを除く）全てのファイルを返すような関数onlyFilesを書いてみてください。
onlyFiles :: Path -> Array Path
onlyFiles file = filter (\x -> not isDirectory x) (allFiles' file)

findFileBySizeCondition :: (Int -> Int -> Boolean) -> Path -> Maybe Path
findFileBySizeCondition compareF file = foldl folder Nothing (onlyFiles file)
  where 
    folder :: Maybe Path -> Path -> Maybe Path
    folder Nothing cur = Just cur
    folder (Just acc) cur = if (fromMaybe 0 $ size cur) `compareF` (fromMaybe 0 $ size acc) then (Just cur) else (Just acc)

maxSizeFile :: Path -> Maybe Path
maxSizeFile file = findFileBySizeCondition (>=) file

minSizeFile :: Path -> Maybe Path
minSizeFile file = findFileBySizeCondition (<=) file

whereIs :: String -> Maybe Path
whereIs path = whereIs' root path
  where 
    whereIs' :: Path -> String -> Maybe Path
    whereIs' path targetFileName = head $ do
      child <- ls path
      guard $ not (isDirectory child)
      guard $ (filename child) == targetFileName
      pure path