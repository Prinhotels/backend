module Paths_thrift (
    version,
    getBinDir, getLibDir, getDataDir, getLibexecDir,
    getDataFileName
  ) where

import qualified Control.Exception as Exception
import Data.Version (Version(..))
import System.Environment (getEnv)
import Prelude

catchIO :: IO a -> (Exception.IOException -> IO a) -> IO a
catchIO = Exception.catch


version :: Version
version = Version {versionBranch = [0,9,0], versionTags = []}
bindir, libdir, datadir, libexecdir :: FilePath

bindir     = "/home/paxer/.cabal/bin"
libdir     = "/home/paxer/.cabal/lib/thrift-0.9.0/ghc-7.4.2"
datadir    = "/home/paxer/.cabal/share/thrift-0.9.0"
libexecdir = "/home/paxer/.cabal/libexec"

getBinDir, getLibDir, getDataDir, getLibexecDir :: IO FilePath
getBinDir = catchIO (getEnv "thrift_bindir") (\_ -> return bindir)
getLibDir = catchIO (getEnv "thrift_libdir") (\_ -> return libdir)
getDataDir = catchIO (getEnv "thrift_datadir") (\_ -> return datadir)
getLibexecDir = catchIO (getEnv "thrift_libexecdir") (\_ -> return libexecdir)

getDataFileName :: FilePath -> IO FilePath
getDataFileName name = do
  dir <- getDataDir
  return (dir ++ "/" ++ name)
