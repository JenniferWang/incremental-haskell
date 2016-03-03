module StabilizationNum
  where
import Types

newStbNum :: StabilizationNum
newStbNum = 0

isNone :: StabilizationNum -> Bool
isNone = (==) none

isSome :: StabilizationNum -> Bool
isSome = (>= 0)

add1 :: StabilizationNum -> StabilizationNum
add1 = (+ 1)

