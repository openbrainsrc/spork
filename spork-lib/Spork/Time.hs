module Spork.Time where

import Data.Time
import Data.Time.Calendar.WeekDate

import Spork.Database

import GHC.Conc

--using seconds as jitter. sub-second reolution not available from instgram
toDayOfWeekJitter :: UTCTime -> Double
toDayOfWeekJitter tm =
  let (_,_,dow) = toWeekDate (utctDay tm)
      secs = round (utctDayTime tm) `mod` 60
      secFrac = realToFrac (secs::Int) / 60 
  in realToFrac dow + secFrac * 0.70 - 0.35 

toDayOfWeek :: UTCTime -> Int
toDayOfWeek tm =
  let (_,_,dow) = toWeekDate (utctDay tm)
  in dow 

toHour :: UTCTime -> Double
toHour = (/3600) . realToFrac . utctDayTime 

hoursAfterY2K :: UTCTime -> Int
hoursAfterY2K tm = round difft where
  y2k = UTCTime (fromGregorian 2000 1 1) 0
  difft = diffUTCTime tm y2k

delaySecs :: Double -> DBC c ()
delaySecs secs = liftIO $ threadDelay (round $ secs * 1000000)
