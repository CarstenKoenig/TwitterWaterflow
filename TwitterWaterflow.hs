-- | TwitterWaterflow.hs
-- | first go at the infamous Twitter Waterflow problem

module TwitterWaterflow 
    ( Height, Volume, Landscape
    , floodFill
    , TestCase
    , testCases
    , checkCase
    , checkAll
    , main
    ) where

import Data.Map.Strict ((!), member, empty)
import qualified Data.Map.Strict as M

import Control.Monad (forM_)

type Height    = Int
type Volume    = Int
type Landscape = [Height]

type FillMap   = M.Map Height Volume

-- | solves the problem by remembering the left-walls height
-- | and using a map to memoize the possible fills for right-wall heights
floodFill :: Landscape -> Volume
floodFill = solve 0 empty (-1)
    where solve filled _ _ [] = filled
          solve filled fillM leftH (h:ls')
            | h >= leftH      = solve filled' empty  h     ls'
            | otherwise       = solve filled  fillM' leftH ls'
            where filled'     = filled + getFill fillM h
                  fillM'      = adjust fillM leftH h

getFill :: FillMap -> Height -> Volume
getFill m h
    | h <= 0       = 0
    | h `member` m = m!h
    | otherwise    = getFill m (h-1)

adjust :: FillMap -> Height -> Height -> FillMap
adjust m maxH curH = foldl add m [curH+1 .. maxH]
    where add m' h  = M.alter adj h m'
                      where adj (Just v) = Just $ v + (h - curH)
                            adj Nothing  = Just $ h - curH

type TestCase = (Landscape, Volume)

testCase1 :: TestCase
testCase1 = ([2, 5, 1, 2, 3, 4, 7, 7, 6], 10)

testCase2 :: TestCase
testCase2 = ([2, 7, 2, 7, 4, 7, 1, 7, 3, 7], 18)

testCase3 :: TestCase
testCase3 = ([2, 5, 1, 3, 1, 2, 1, 7, 7, 6], 17)

testCase4 :: TestCase
testCase4 = ([2, 3, 4, 5, 5, 4, 3], 0)

testCase5:: TestCase
testCase5 = ([5, 4, 3, 3, 4, 5], 6)

testCase6 :: TestCase
testCase6 = ([5, 0, 0], 0)

testCases :: [TestCase]
testCases = [testCase1, testCase2, testCase3, testCase4, testCase5, testCase6]

checkCase :: TestCase -> (Volume, Bool)
checkCase (ls, expected) = (actual, expected == actual)
    where actual = floodFill ls

checkAll :: IO()
checkAll = forM_ testCases check
    where check (c@(_,ex)) = do
            putStr $ "checking landscape with colume " ++ show ex ++ "..."
            let (vol, ok) = checkCase c
            putStrLn (if ok then "OK" else "FAILED with volume " ++ show vol)

main :: IO()
main = checkAll
