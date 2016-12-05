type Triangle = (Int, Int, Int)

fakeTriangle :: Triangle
fakeTriangle = (1, 1, 100)

asTriangle :: [String] -> Triangle
asTriangle (a:b:c:_) = (read a, read b, read c)
asTriangle _ = fakeTriangle

isPossible :: Triangle -> Bool
isPossible (a, b, c) = a + b > c && a + c > b && b + c > a

parseBy3 :: [[String]] -> [Triangle]
parseBy3 (l:m:n:rest) = asT 0 : asT 1 : asT 2 : parseBy3 rest
  where
    asT i = asTriangle [l!!i, m!!i, n!!i]
parseBy3 _ = []

main :: IO()
main = do
  input <- readFile "./day03/input.txt"
  let numbers = map words $ lines input
  part1 numbers
  part2 numbers

part1 :: [[String]] -> IO()
part1 numbers =
  print $ show $ length $ filter isPossible triangles
  where
    triangles = map asTriangle numbers

part2 :: [[String]] -> IO()
part2 numbers =
  print $ show $ length $ filter isPossible triangles
  where
    triangles = parseBy3 numbers
