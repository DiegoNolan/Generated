module Paths_Generated (
    version,
    getBinDir, getLibDir, getDataDir, getLibexecDir,
    getDataFileName
  ) where

import qualified Control.Exception as Exception
import Data.Version (Version(..))
import System.Environment (getEnv)
catchIO :: IO a -> (Exception.IOException -> IO a) -> IO a
catchIO = Exception.catch


version :: Version
version = Version {versionBranch = [0,1,0,0], versionTags = []}
bindir, libdir, datadir, libexecdir :: FilePath

bindir     = "/home/nolan/.cabal/bin"
libdir     = "/home/nolan/.cabal/lib/Generated-0.1.0.0/ghc-7.4.2"
datadir    = "/home/nolan/.cabal/share/Generated-0.1.0.0"
libexecdir = "/home/nolan/.cabal/libexec"

getBinDir, getLibDir, getDataDir, getLibexecDir :: IO FilePath
getBinDir = catchIO (getEnv "Generated_bindir") (\_ -> return bindir)
getLibDir = catchIO (getEnv "Generated_libdir") (\_ -> return libdir)
getDataDir = catchIO (getEnv "Generated_datadir") (\_ -> return datadir)
getLibexecDir = catchIO (getEnv "Generated_libexecdir") (\_ -> return libexecdir)

getDataFileName :: FilePath -> IO FilePath
getDataFileName name = do
  dir <- getDataDir
  return (dir ++ "/" ++ name)
