{-# LANGUAGE Arrows #-}
{-# LANGUAGE RecordWildCards #-}
module ParametricController where

import FRP.Yampa
import TORCS

import SettingType
import Selector
import FunctionDefs

runController :: (Setting , Setting, Setting)-> IO (CarState,DriveState)
runController (g_sel, s_sel, a_sel) = (startDriver $ myDriver g_sel s_sel a_sel 70)


myDriver :: Setting -> Setting -> Setting -> Double -> Driver
myDriver g_sel s_sel a_sel targetSpeed = proc c@CarState{..} -> do

    g <- selectFromCaseDII g_sel targetSpeed -< c
    s <- selectFromCaseDDD s_sel targetSpeed -< c
    a <- selectFromCaseDDDPrev a_sel targetSpeed -< (c,s)

    m <- arr endRace -< (lapTimes,curLapTime)
    returnA -< defaultDriveState {accel = a, gear = g, steer = s, meta = m}
