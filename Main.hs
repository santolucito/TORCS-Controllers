module Main where

import TORCS
import Control.Applicative

import ParametricController
import SettingType

g_sel = Setting "shifting" "(rpm,gear')"
s_sel = Setting "steering" "(angle,trackPos)"
a_sel = Setting "gas" "(speedX,s)"

-- Settings of type SF _ (Double,Double)
allDoubleInputs = ["angle","trackPos","speedY","speedX","speedZ","rpm"]
allDoubleInputPairs = 
  map (\(x,y)-> "("++x++","++y++")") $ pairs allDoubleInputs
allDoubleDoubleFxns = ["steering","gas"]
allDoubleDoubleSettings = liftA2 (Setting) allDoubleDoubleFxns allDoubleInputPairs
allSPairs = map (\x->"("++x++",s)") allDoubleInputs
indirectDoubleDoubleSettings = liftA2 (Setting) allDoubleDoubleFxns allSPairs

-- Settings of type SF _ (Double,Int)
allDoubleIntInputPairs =
  map (\x->"("++x++",gear')") allDoubleInputs
allShiftSettings = map (Setting "shifting") allDoubleIntInputPairs

-- | all the paths that do not reuse a computed output
--   every one of these could be applied to any output signal
--   this is all the 'well-typed' settings using only direct input
allDirectSettings = allShiftSettings++allDoubleDoubleSettings

allThreePairs = length $ liftA3 (,,) allDirectSettings allDirectSettings (allDirectSettings++indirectDoubleDoubleSettings)

main :: IO()
main = do
  (cS,dS) <- runController (g_sel, s_sel, a_sel)
  print $ damage cS
  

pairs :: [a]  -> [(a,a)]
pairs xs = liftA2 (,) xs xs
