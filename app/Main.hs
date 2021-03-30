module Main
    ( main
    ) where

import RIO

import Lockit
import Lockit.App
import Lockit.GitHub
import Lockit.Time

main :: IO ()
main = do
    app <- loadApp

    runAppT app $ do
        cutoff <- liftIO $ subtractDays (Days 30) <$> getCurrentTime
        run (mkOwnerName "pbrisbin") (mkRepoName "aurget") cutoff
