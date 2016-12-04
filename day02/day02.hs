type Button = Int
data Dir = U | D | L | R | Z deriving (Show)

sizeH :: Int
sizeH = 3

sizeV :: Int
sizeV = 3

asDir :: Char -> Dir
asDir 'U' = U
asDir 'R' = R
asDir 'D' = D
asDir 'L' = L
asDir _ = Z

nextButton :: Button -> Dir -> Button
nextButton b U
  | getRow b > 1 = b - sizeH
  | otherwise = b
nextButton b R
  | getCol b < sizeH = b + 1
  | otherwise = b
nextButton b D
  | getRow b < sizeV = b + sizeH
  | otherwise = b
nextButton b L
  | getCol b > 1 = b - 1
  | otherwise = b
nextButton b _ = b

nextButtonStar :: Button -> Dir -> Button
nextButtonStar 3 U = 3
nextButtonStar 3 R = 3
nextButtonStar 3 L = 3
nextButtonStar 7 U = 7
nextButtonStar 7 L = 7
nextButtonStar 9 U = 9
nextButtonStar 9 R = 9
nextButtonStar 11 U = 11
nextButtonStar 11 D = 11
nextButtonStar 11 L = 11
nextButtonStar 15 U = 15
nextButtonStar 15 R = 15
nextButtonStar 15 D = 15
nextButtonStar 17 L = 17
nextButtonStar 17 D = 17
nextButtonStar 19 R = 19
nextButtonStar 19 D = 19
nextButtonStar 23 R = 23
nextButtonStar 23 D = 23
nextButtonStar 23 L = 23
nextButtonStar b U = b - 5
nextButtonStar b R = b + 1
nextButtonStar b D = b + 5
nextButtonStar b L = b - 1
nextButtonStar b _ = b

followInstructions :: (Button -> Dir -> Button) -> Button -> [[Dir]] -> [Button]
followInstructions _ _ [] = []
followInstructions foldFn b (dirs:dirss) = nb : followInstructions foldFn nb dirss
  where nb = foldl foldFn b dirs

getCol :: Button -> Int
getCol b = ((b - 1) `mod` sizeH) + 1

getRow :: Button -> Int
getRow b = ((b - 1) `div` sizeH) + 1

getVisualCodeStar :: Button -> Char
getVisualCodeStar 3 = '1'
getVisualCodeStar 7 = '2'
getVisualCodeStar 8 = '3'
getVisualCodeStar 9 = '4'
getVisualCodeStar 11 = '5'
getVisualCodeStar 12 = '6'
getVisualCodeStar 13 = '7'
getVisualCodeStar 14 = '8'
getVisualCodeStar 15 = '9'
getVisualCodeStar 17 = 'A'
getVisualCodeStar 18 = 'B'
getVisualCodeStar 19 = 'C'
getVisualCodeStar 23 = 'D'
getVisualCodeStar _ = '?'

main :: IO()
main = do
  input <- readFile "./day02/input.txt"
  let directionss = map (map asDir) $ lines input
  part1 directionss
  part2 directionss

part1 :: [[Dir]] -> IO()
part1 directionss =
  print $ show $ followInstructions nextButton 5 directionss

part2 :: [[Dir]] -> IO()
part2 directionss =
  print $ map getVisualCodeStar $ followInstructions nextButtonStar 11 directionss
