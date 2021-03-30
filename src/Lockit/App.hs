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

instance HasGitHubAuth App where
    githubAuthL = lens appGitHubAuth $ \x y -> x { appGitHubAuth = y }

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
    deriving MonadGitHub via ActualGitHub (AppT m)

runAppT :: App -> AppT m a -> m a
runAppT app f = runReaderT (unAppT f) app
