module Paths_deepnet (
    version,
    getBinDir, getLibDir, getDataDir, getLibexecDir,
    getDataFileName, getSysconfDir
  ) where

import qualified Control.Exception as Exception
import Data.Version (Version(..))
import System.Environment (getEnv)
import Prelude

catchIO :: IO a -> (Exception.IOException -> IO a) -> IO a
catchIO = Exception.catch

version :: Version
version = Version [0,1,0,0] []
bindir, libdir, datadir, libexecdir, sysconfdir :: FilePath

bindir     = "/Users/Ethan/deepnet/.stack-work/install/x86_64-osx/lts-4.0/7.10.3/bin"
libdir     = "/Users/Ethan/deepnet/.stack-work/install/x86_64-osx/lts-4.0/7.10.3/lib/x86_64-osx-ghc-7.10.3/deepnet-0.1.0.0-98vzwQjA0spLmxroj6F8bT"
datadir    = "/Users/Ethan/deepnet/.stack-work/install/x86_64-osx/lts-4.0/7.10.3/share/x86_64-osx-ghc-7.10.3/deepnet-0.1.0.0"
libexecdir = "/Users/Ethan/deepnet/.stack-work/install/x86_64-osx/lts-4.0/7.10.3/libexec"
sysconfdir = "/Users/Ethan/deepnet/.stack-work/install/x86_64-osx/lts-4.0/7.10.3/etc"

getBinDir, getLibDir, getDataDir, getLibexecDir, getSysconfDir :: IO FilePath
getBinDir = catchIO (getEnv "deepnet_bindir") (\_ -> return bindir)
getLibDir = catchIO (getEnv "deepnet_libdir") (\_ -> return libdir)
getDataDir = catchIO (getEnv "deepnet_datadir") (\_ -> return datadir)
getLibexecDir = catchIO (getEnv "deepnet_libexecdir") (\_ -> return libexecdir)
getSysconfDir = catchIO (getEnv "deepnet_sysconfdir") (\_ -> return sysconfdir)

getDataFileName :: FilePath -> IO FilePath
getDataFileName name = do
  dir <- getDataDir
  return (dir ++ "/" ++ name)
