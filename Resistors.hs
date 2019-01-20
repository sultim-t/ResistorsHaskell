module Resistors (
  Resistor,
  Connection,
  calcResistance ) where

-- Types --
type Resistor = Float
type Position = (Int, Int)

data Connection = NoConnections
                | Series Resistor Connection
                | Parallel Connection Connection

data Block = BEmpty | BResistor
           | BDashH | BDashV
           | BParL
           | BParInR | BParOutR
           deriving Show

-- Calculate resistance --
calcResistance :: Connection -> Float
calcResistance NoConnections = 0
calcResistance (Series r c) = r + calcResistance c
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
connToStr c = blocksToStr $ connToBlocks c $ countColumns c

connToBlocks :: Connection -> Int -> [[Block]]
connToBlocks NoConnections _ = [[]]
connToBlocks (Series _ c) width = [BResistor] : connToBlocks c width
connToBlocks (Parallel c1 c2) width = [[BParL] ++ replicate (width - 2) BDashH ++ [BParInR]]
                                   ++ mergeBlocks (connToBlocks c1 (width - 1))
                                                  (connToBlocks c2 (width - 1))
                                   ++ [[BParL] ++ replicate (width - 2) BDashH ++ [BParOutR]]

mergeBlocks :: [[Block]] -> [[Block]] -> [[Block]]
mergeBlocks a [] = a
mergeBlocks [] b = b
mergeBlocks (x:xs) (y:ys) = concat (x : [y]) : mergeBlocks xs ys

positions :: Connection -> Int -> Int -> [(Block, Position)]
positions NoConnections _ _ = []
positions (Series _ c) x y = (BResistor, (x, y)) : positions c x (y + 1)
positions (Parallel c1 c2) x y = [(BParL, (x, y))]
                                ++ fill BDashH (x + 1, y) (countColumns c1 - 1)
                                ++ [(BParInR, (x + countColumns c1, y))]
                                ++ positions c1 x (y + 1) ++ positions c2 (x + countColumns c1) (y + 1)

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

-- Blocks to string --
blocksToStr :: [[Block]] -> String
blocksToStr blocks = foldr ((++) . blocksRowToStr) "" blocks

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
blockToStr BResistor = [" +-+",
                        " | |",
                        " +-+",
                        "  | "]
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
blockToStr BParOutR  = ["  | ",
                        "--+ ",
                        "    ",
                        "    "]

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
t6 = Parallel (Parallel (Series 0 NoConnections)
                        (Parallel (Series 0 NoConnections) (Series 0 NoConnections)))
              (Parallel (Series 2 NoConnections) (Series 3 NoConnections))

t7 :: Connection
t7 = Parallel (Parallel (Parallel (Series 0 NoConnections) (Series 0 NoConnections))
                        (Parallel (Series 0 NoConnections) (Series 0 NoConnections)))
              (Parallel (Parallel (Series 0 NoConnections) (Series 0 NoConnections))
                        (Parallel (Series 0 NoConnections) (Series 0 NoConnections)))

tw1 :: Connection
tw1 =  Parallel (Series 0 NoConnections) (Series 3 NoConnections)

tw2 :: Connection
tw2 =  Parallel NoConnections (Series 3 NoConnections)
