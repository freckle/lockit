module LockitSpec
    ( spec
    ) where

import RIO

import Control.Monad.State
import Lockit
import Lockit.GitHub
import Lockit.GitHub.Test
import Lockit.Time
import Test.Hspec

newtype TestAppT m a = TestAppT
    { unTestAppT :: StateT IssueMap m a
    }
    deriving newtype
        ( Functor
        , Applicative
        , Monad
        , MonadIO
        , MonadState IssueMap
        )

runTestAppT :: Monad m => IssueMap -> TestAppT m a -> m a
runTestAppT issues f = evalStateT (unTestAppT f) issues

execTestAppT :: Monad m => IssueMap -> TestAppT m a -> m IssueMap
execTestAppT issues f = execStateT (unTestAppT f) issues

spec :: Spec
spec = do
    describe "run" $ do
        it "does not error" $ example $ do
            let issues = makeIssueMap []

            runTestAppT issues $ run testOrg testRepo $ makeUTCTime 2020 1 1

        it "locks issues closed earlier than a cutoff date" $ example $ do
            let
                issues = makeIssueMap
                    [ Issue (mkIssueId 1) False Nothing
                    , Issue (mkIssueId 2) False $ Just $ makeUTCTime 2020 1 1
                    , Issue (mkIssueId 3) False $ Just $ makeUTCTime 2020 1 3
                    , Issue (mkIssueId 4) False $ Just $ makeUTCTime 2020 1 5
                    , Issue (mkIssueId 5) False $ Just $ makeUTCTime 2020 1 6
                    , Issue (mkIssueId 6) True Nothing
                    ]

            updated <- execTestAppT issues $ do
                run testOrg testRepo $ makeUTCTime 2020 1 3

            fmap issueLocked (lookupIssue (mkIssueId 1) updated)
                `shouldBe` Just False
            fmap issueLocked (lookupIssue (mkIssueId 2) updated)
                `shouldBe` Just True
            fmap issueLocked (lookupIssue (mkIssueId 3) updated)
                `shouldBe` Just True
            fmap issueLocked (lookupIssue (mkIssueId 4) updated)
                `shouldBe` Just False
            fmap issueLocked (lookupIssue (mkIssueId 5) updated)
                `shouldBe` Just False
            fmap issueLocked (lookupIssue (mkIssueId 6) updated)
                `shouldBe` Just True

testOrg :: Name Owner
testOrg = mkOwnerName "foo"

testRepo :: Name Repo
testRepo = mkRepoName "bar"

makeUTCTime :: Integer -> Int -> Int -> UTCTime
makeUTCTime year month day =
    UTCTime { utctDay = fromGregorian year month day, utctDayTime = 0 }
