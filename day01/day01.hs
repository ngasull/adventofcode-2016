import Data.List.Split

data TurnDir = L | R | Z
data Heading = N | E | S | W
data Instruction = Instruction { dir :: TurnDir
                               , distance :: Int
                               }

data State = State { x :: Int
                   , y :: Int
                   , heading :: Heading
                   }

instance Eq State where
  State{x=x1, y=y1} == State{x=x2, y=y2} = x1 == x2 && y1 == y2

asInstruction :: String -> Instruction
asInstruction str = parseInstruction (head str) (tail str)
  where
    parseInstruction 'L' nChar = Instruction{dir=L, distance=read nChar}
    parseInstruction 'R' nChar = Instruction{dir=R, distance=read nChar}
    parseInstruction _ nChar = Instruction{dir=Z, distance=read nChar}

updateState :: State -> Instruction -> State
updateState State{heading=h, x=sx, y=sy} Instruction{dir=idir, distance=dist} =
  State{heading=newHeading, x=sx+dx, y=sy+dy}
  where
    newHeading = updateHeading idir h
    (dx, dy) = getPositionDiff newHeading dist

updateHeading :: TurnDir -> Heading -> Heading
updateHeading L N = W
updateHeading R N = E
updateHeading L E = N
updateHeading R E = S
updateHeading L S = E
updateHeading R S = W
updateHeading L W = S
updateHeading R W = N
updateHeading Z h = h

getPositionDiff :: Heading -> Int -> (Int, Int)
getPositionDiff N dist = (0, dist)
getPositionDiff E dist = (dist, 0)
getPositionDiff S dist = (0, -dist)
getPositionDiff W dist = (-dist, 0)

stateDistance :: State -> Int
stateDistance State{x=sx, y=sy} = abs sx + abs sy

unitarizeInstructions :: [Instruction] -> [Instruction]
unitarizeInstructions [] = []
unitarizeInstructions (i:is) = unitarize i ++ unitarizeInstructions is
  where
    unitarize Instruction{dir=_, distance=0} = []
    unitarize Instruction{dir=d, distance=dist} =
      Instruction{dir=d, distance=1} : unitarize Instruction{dir=Z, distance=dist-1}

findFirstCrossedTwice :: [State] -> Maybe State
findFirstCrossedTwice [] = Nothing
findFirstCrossedTwice (s:ss)
  | s `elem` ss = Just s
  | otherwise = findFirstCrossedTwice ss

main :: IO()
main = do
  input <- readFile "./day01/input.txt"
  let instructions = map asInstruction $ splitOn ", " input
  let initialState = State{heading=N, x=0, y=0}
  part1 initialState instructions
  part2 initialState instructions

part1 :: State -> [Instruction] -> IO()
part1 initialState instructions = do
  print "Part 1 - Full distance"
  printState $ foldl updateState initialState instructions

part2 :: State -> [Instruction] -> IO()
part2 initialState instructions = do
  print "Part 2 - First location visited twice"
  handleResult $ findFirstCrossedTwice statesList
  where
    statesList = scanl updateState initialState $ unitarizeInstructions instructions
    handleResult Nothing = print "No location crossed twice"
    handleResult (Just s) = printState s

printState :: State -> IO()
printState s@State{x=sx, y=sy} =
  print $ show sx ++ " " ++ show sy ++ " -> " ++ show (stateDistance s)
