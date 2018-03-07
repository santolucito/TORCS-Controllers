module FormatInput where

import System.Directory
import Data.List.Split
  
outDir = "cleanedTrainingSet_DistRaced/"

formatInput :: FilePath -> IO ()
formatInput dirPath = do
  allFs <- listDirectory dirPath
  mapM_ formatOne $ map (dirPath++) allFs

formatOne :: FilePath -> IO()
formatOne fp = do
  fc <- readFile fp
  let cfs = settingsToCodeFeatures $ head $ lines fc
  let labels = outsToLabels $ head $ tail $ lines fc
  --print $ head $ tail $ lines fc
  --putStrLn (labels!!0)
  writeFile (outDir++(concat cfs)) (unlines $ cfs++labels)

settingsToCodeFeatures :: String -> [String]
settingsToCodeFeatures s = let
  settings = map stripFunnyChars $ splitOn "\"," $drop 1 $ take (length s-1) s
 in
  zipWith (++) ["gear<-","accel<-","steer<-"] settings

--TODO just output CarState as json in generation so we can read it here
outsToLabels :: String -> [String]
outsToLabels s = let 
  fields = splitOn ", " $ stripFunnyChars s
  fPairs = map ((\xs -> (head xs,head $ tail xs)).splitOn " = ") fields
 in
  --[tooMuchDamage$ snd $ fPairs !! 7]
  [madeProgress $ snd $ fPairs !! 5]
  
 
stripFunnyChars :: String -> String
stripFunnyChars = filter (\c-> c/='\"' && c/='\'')

madeProgress :: String -> String
madeProgress s = 
  if read s >= 0.0
  then "madeProgress"
  else "noProgress"

tooMuchDamage :: String -> String
tooMuchDamage s = 
  if read s > 0.0
  then "tookDamage"
  else "noDamage"
