module Main where

import TORCS
import Control.Applicative

import System.Random
import Data.Array.IO
import Control.Monad

import ParametricController
import SettingType

-- Settings of type SF _ (Double,Double)
allDoubleInputs = ["angle","trackPos","speedY","speedX","speedZ","rpm"]
allDoubleInputPairs = 
  map (\(x,y)-> "("++x++","++y++")") $ pairs allDoubleInputs
allDDFxns = ["steering","(gas targetSpeed)"]
allDDSettings = liftA2 (Setting) allDDFxns allDoubleInputPairs
allSPairs = map (\x->"("++x++",s)") allDoubleInputs
indirectDDSettings = liftA2 (Setting) allDDFxns allSPairs

-- Settings of type SF _ (Double,Int)
allDoubleIntInputPairs =
  map (\x->"("++x++",gear')") allDoubleInputs
allShiftSettings = map (Setting "shifting") allDoubleIntInputPairs

-- | all the paths that do not reuse a computed output
--   every one of these could be applied to any output signal
--   this is all the 'well-typed' settings using only direct input
allDirectSettings = allDDSettings

allThreePairs = liftA3 (,,) allShiftSettings allDirectSettings (allDirectSettings++indirectDDSettings)
fst3 (x,y,z) = x

main :: IO()
main = do
  randomOrderTestCases <- shuffle allThreePairs
  mapM getDataPoint randomOrderTestCases
  return ()

getDataPoint threePair = do
  (cS,dS) <- runController threePair
  writeFile ("trainingSetGen/"++(filter (\c-> c/=' ' && c/='\"' ) $ show threePair)) ((show threePair)++"\n"++(show cS))
  return cS
  

-- | Randomly shuffle a list
--   /O(N)/ https://wiki.haskell.org/Random_shuffle
shuffle :: [a] -> IO [a]
shuffle xs = do
        ar <- newArray n xs
        forM [1..n] $ \i -> do
            j <- randomRIO (i,n)
            vi <- readArray ar i
            vj <- readArray ar j
            writeArray ar j vi
            return vj
  where
    n = length xs
    newArray :: Int -> [a] -> IO (IOArray Int a)
    newArray n xs =  newListArray (1,n) xs


pairs :: [a]  -> [(a,a)]
pairs xs = liftA2 (,) xs xs
