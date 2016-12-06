import Data.Function
import Data.List
import qualified Data.Map as M

main :: IO()
main = do
  input <- readFile "./day06/input.txt"
  let messages = lines input
  part1 messages
  part2 messages

part1 :: [String] -> IO()
part1 messages =
  print $ map (\col -> fst $ maximumBy (compare `on` snd) $ M.toList $ M.fromListWith (+) [(c, 1) | c <- col]) $ transpose messages

part2 :: [String] -> IO()
part2 messages =
  print $ map (\col -> fst $ minimumBy (compare `on` snd) $ M.toList $ M.fromListWith (+) [(c, 1) | c <- col]) $ transpose messages
