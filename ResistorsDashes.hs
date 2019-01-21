module Resistors (
  Resistor,
  Connection,
  calcResistance ) where

import Data.List (sortBy, nub, delete)


-- Types --
type Resistor = Float
type Position = (Int, Int)

data Connection = NoConnections
                | Series Resistor Connection
                | Parallel Connection Connection

data Block = BEmpty | BResistor Int
           | BDashH | BDashV
           | BParL
           | BParInR | BParOutR
           deriving Show

instance Eq Block where
  BEmpty == BEmpty = True
  BResistor _ == BResistor _ = True
  BDashH == BDashH = True
  BDashV == BDashV = True
  BParL == BParL = True
  BParInR == BParInR = True
  BParOutR == BParOutR = True
  _ == _ = False

-- Calculate resistance --
calcResistance :: Connection -> Float
calcResistance NoConnections = 0
calcResistance (Series r c) = r + calcResistance c
calcResistance (Parallel NoConnections c2) = calcResistance c2
calcResistance (Parallel c1 NoConnections) = calcResistance c1
calcResistance (Parallel c1 c2) =
  let r1 = calcResistance c1
      r2 = calcResistance c2
  in if r1 + r2 /= 0
     then (r1 * r2) / (r1 + r2)
     else 0

-- Show connections --
instance Show Connection where
  show = connToStr

connToStr :: Connection -> String
connToStr c = blocksToStr $ posToBlocks $ insertDashV $ positions c 0 0

posToBlocks :: [(Block, Position)] -> [[Block]]
posToBlocks [] = []
posToBlocks l = let sorted = sortBlocks l
                    unused = findUnused sorted [1..(maxRow sorted)]
                    finalPos = removeUnused sorted unused
                in splitToMatrix finalPos [] 0

maxRow :: [(Block, Position)] -> Int
maxRow [] = 0
maxRow ((_, (_, y)):bs) = max y (maxRow bs)

splitToMatrix :: [(Block, Position)] -> [Block] -> Int -> [[Block]]
splitToMatrix [] row _ = [row]
splitToMatrix ((b, (x, y)):xs) row prev = if y == prev
                                          then splitToMatrix xs (addBlock row b x) y
                                          else row : splitToMatrix xs (addBlock [] b x) y

addBlock :: [Block] -> Block -> Int -> [Block]
addBlock xs x n = if length xs < n
                  then xs ++ replicate (n - length xs) BEmpty ++ [x]
                  else xs ++ [x]

sortBlocks :: [(Block, Position)] -> [(Block, Position)]
sortBlocks = sortBy (\(_, xy1) (_, xy2) -> compareYX xy1 xy2)

compareYX :: (Int, Int) -> (Int, Int) -> Ordering
compareYX (x1, y1) (x2, y2) = let y = compare y1 y2 in
                                if y == EQ
                                then compare x1 x2
                                else y

removeUnused :: [(Block, Position)] -> [Int] -> [(Block, Position)]
removeUnused [] _ = []
removeUnused (bl@(_, (_, y)):bs) ns = if y `elem` ns
                                   then removeUnused bs ns
                                   else bl : removeUnused bs ns

findUnused :: [(Block, Position)] -> [Int] -> [Int]
findUnused [] a = a
findUnused ((b, (_, y)):bs) ns = if b /= BEmpty && b /= BDashV
                                 then findUnused bs (delete y ns)
                                 else findUnused bs ns

insertDashV :: [(Block, Position)] -> [(Block, Position)]
insertDashV bs = let ps = filterPos bs $ unique $ findCheckPos bs bs
                 in (bs ++ fmap (\p -> (BDashV, p)) ps)

unique :: Eq a => [a] -> [a]
unique = nub

filterPos :: [(Block, Position)] -> [Position] -> [Position]
filterPos bs = filter (checkPos bs)

findCloseRow :: [(Block, Position)] -> Position -> Int
findCloseRow [] _ = -1
findCloseRow ((b, (x, y)):bs) p@(col, afterRow) = if x == col && y > afterRow && (b == BParL || b == BParOutR)
                                                then y
                                                else findCloseRow bs p

checkPos :: [(Block, Position)] -> Position -> Bool
checkPos [] _ = True
checkPos ((_, (x, y)):xs) p@(px, py) = if y == py && x == px
                                       then False
                                       else checkPos xs p

findCheckPos :: [(Block, Position)] -> [(Block, Position)] -> [Position]
findCheckPos [] _ = []
findCheckPos ((b, (x, y)):xs) origin = if b == BResistor 0 || b == BParL
                                       then fmap (\a -> (x, a)) [(y + 1)..(findCloseRow origin (x, y) - 1)] ++ findCheckPos xs origin
                                       else findCheckPos xs origin

positions :: Connection -> Int -> Int -> [(Block, Position)]
positions NoConnections _ _ = []
positions (Series r c) x y = (BResistor (round r), (x, y)) : positions c x (y + 1)
positions (Parallel c1 c2) x y = [(BParL, (x, y))]
                                ++ fill BDashH (x + 1, y) (countColumns c1 - 1)
                                ++ [(BParInR, (x + countColumns c1, y))]
                                ++ positions c1 x (y + 1) ++ positions c2 (x + countColumns c1) (y + 1)
                                ++ [(BParOutR, (x + countColumns c1, outRow y c1 c2))]
                                ++ fill BDashH (x + 1, outRow y c1 c2) (countColumns c1 - 1)
                                ++ [(BParL, (x, outRow y c1 c2))]

outRow :: Int -> Connection -> Connection -> Int
outRow y c1 c2 = y + 1 + max (countRows c1) (countRows c2)

fill :: Block -> Position -> Int -> [(Block, Position)]
fill b (x, y) count = if count > 0
                      then (b, (x, y)) : fill b (x + 1, y) (count - 1)
                      else []

countParallel :: Connection -> Int
countParallel NoConnections = 0
countParallel (Series _ c) = countParallel c
countParallel (Parallel c1 c2) = 1 + countParallel c1 + countParallel c2

countColumns :: Connection -> Int
countColumns c = 1 + countParallel c

countSeries :: Connection -> Int
countSeries NoConnections = 0
countSeries (Series _ c) = 1 + countSeries c
countSeries (Parallel c1 c2) = countSeries c1 + countSeries c2

countRows :: Connection -> Int
countRows c = countSeries c + 2 * countParallel c

-- Blocks to string --
blocksToStr :: [[Block]] -> String
blocksToStr = foldr ((++) . blocksRowToStr) ""

blocksRowToStr :: [Block] -> String
blocksRowToStr row = concatByBlock $ concatByRow $ blocksRowToStrs row

blocksRowToStrs :: [Block] -> [[String]]
blocksRowToStrs = fmap blockToStr

concatByRow :: [[String]] -> [String]
concatByRow [] = []
concatByRow x = concat (heads x) : concatByRow (noEmpty $ noHeads x)

heads :: [[a]] -> [a]
heads = fmap head

noHeads :: [[a]] -> [[a]]
noHeads = fmap tail

noEmpty :: [[a]] -> [[a]]
noEmpty [] = []
noEmpty (x:xs) = if null x
                 then noEmpty xs
                 else x : noEmpty xs

concatByBlock :: [String] -> String
concatByBlock [] = ""
concatByBlock (x:xs) = x ++ "\n" ++ concatByBlock xs

blockToStr :: Block -> [String]
blockToStr BEmpty    = ["    ",
                        "    ",
                        "    ",
                        "    "]
blockToStr BDashH    = ["    ",
                        "----",
                        "    ",
                        "    "]
blockToStr BDashV    = ["  | ",
                        "  | ",
                        "  | ",
                        "  | "]
blockToStr BParL     = ["  | ",
                       "  +-",
                        "  | ",
                        "  | "]
blockToStr BParInR   = ["    ",
                        "--+ ",
                        "  | ",
                        "  | "]
blockToStr BParOutR   = ["  | ",
                        "--+ ",
                        "    ",
                        "    "]

blockToStr (BResistor n) = [" +-+", insertNumber n, " +-+", "  | "]

insertNumber :: Int -> String
insertNumber n | n >= 0 && n <= 9 = " |" ++ show n ++ "|"
               | otherwise        = " | |"

-- tests
t1 :: Connection
t1 = Series 1 (Series 2 NoConnections)

t2 :: Connection
t2 = Parallel (Series 2 NoConnections) (Series 4 NoConnections)

t3 :: Connection
t3 = Series 1 (Parallel (Series 2 NoConnections) (Series 3 NoConnections))

t4 :: Connection
t4 = Parallel (Parallel (Series 2 NoConnections) (Series 3 NoConnections))
              (Parallel (Series 2 NoConnections) (Series 3 NoConnections))

t5 :: Connection
t5 = Series 1 $ Parallel (Series 2 $ Series 1 $ Parallel (Series 1 NoConnections) (Series 6 NoConnections))
                         (Series 4 NoConnections)

t6 :: Connection
t6 = Parallel (Parallel (Series 9 NoConnections)
                        (Parallel (Series 1 NoConnections) (Series 6 NoConnections)))
              (Parallel (Series 2 NoConnections) (Series 3 NoConnections))

t7 :: Connection
t7 = Parallel (Parallel (Parallel (Series 1 NoConnections) (Series 7 NoConnections))
                        (Parallel (Series 2 NoConnections) (Series 6 NoConnections)))
              (Parallel (Parallel (Series 13 NoConnections) (Series 3 NoConnections))
                        (Parallel (Series 2 NoConnections) (Series 3 NoConnections)))

tw1 :: Connection
tw1 =  Parallel (Series 0 NoConnections) (Series 3 NoConnections)

tw2 :: Connection
tw2 =  Parallel NoConnections (Series 3 NoConnections)
