module Lockit.App
    ( App
    , loadApp
    , AppT
    , runAppT
    ) where

import RIO

import qualified Data.ByteString.Char8 as BS8
import GitHub
import Lockit.GitHub
import System.Environment

data App = App
    { appGitHubAuth :: Auth
    , appLogFunc :: LogFunc
    }

instance HasLogFunc App where
    logFuncL = lens appLogFunc $ \x y -> x { appLogFunc = y }

loadApp :: LogFunc -> IO App
loadApp lf = do
    token <- getEnv "GITHUB_TOKEN"
    pure App { appGitHubAuth = OAuth $ BS8.pack token, appLogFunc = lf }

newtype AppT m a = AppT
    { unAppT :: ReaderT App m a
    }
    deriving newtype
        ( Functor
        , Applicative
        , Monad
        , MonadIO
        , MonadReader App
        )

instance MonadIO m => MonadGitHub (AppT m) where
    fetchClosedIssues org repo = do
        auth <- asks appGitHubAuth
        result <- liftIO $ github auth $ issuesForRepoR
            org
            repo
            stateClosed
            FetchAll
        either throwIO (pure . fromIssuesResponse) result

    -- Haskell github package lacks API
    lockIssue issue = logInfo $ "Locking Issue: " <> display issue

runAppT :: App -> AppT m a -> m a
runAppT app f = runReaderT (unAppT f) app
