module Lockit.Time
    ( Days(..)
    , subtractDays
    , module RIO.Time
    ) where

import RIO

import RIO.Time

newtype Days = Days
    { unDays :: Natural
    }

subtractDays :: Days -> UTCTime -> UTCTime
subtractDays = undefined
