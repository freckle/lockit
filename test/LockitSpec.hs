module LockitSpec
    ( spec
    ) where

import RIO

import Data.Time.Calendar
import Lockit
import Lockit.GitHub
import Lockit.Time
import Test.Hspec

data TestApp = TestApp

spec :: Spec
spec = do
    describe "run" $ do
        it "does not error" $ example $ do
            runRIO TestApp $ run testOrg testRepo $ makeUTCTime 2020 1 1

testOrg :: Name Owner
testOrg = mkOwnerName "foo"

testRepo :: Name Repo
testRepo = mkRepoName "bar"

makeUTCTime :: Integer -> Int -> Int -> UTCTime
makeUTCTime year month day =
    UTCTime { utctDay = fromGregorian year month day, utctDayTime = 0 }
