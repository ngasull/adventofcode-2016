import Crypto.Hash
import qualified Data.ByteString as B
import qualified Data.ByteString.Char8 as BC
import Data.Function
import Data.List

md5 :: String -> String
md5 = show . (hash :: B.ByteString -> Digest MD5) . BC.pack

main :: IO()
main = do
  let input = "abc"
  let fiveZeroesHashes = filter (all (== '0') . take 5) $ map (md5 . (input ++) . show) [0..]
  part1 fiveZeroesHashes
  part2 fiveZeroesHashes

part1 :: [String] -> IO()
part1 fiveZeroesHashes =
  print $ take 8 $ map (!!5) fiveZeroesHashes

part2 :: [String] -> IO()
part2 fiveZeroesHashes =
  print $ map snd $ sortBy (compare `on` fst) $ take 8 positionnedChars
  where
    positionnedChars = removeDupes [] $ filter hasValidPosition $ map (\h -> (h!!5, h!!6)) fiveZeroesHashes
    hasValidPosition (p, _) = p `elem` ['0'..'7']
    removeDupes _ [] = []
    removeDupes positions (t@(p, _):ts)
      | p `elem` positions = removeDupes positions ts
      | otherwise = t : removeDupes (p : positions) ts
