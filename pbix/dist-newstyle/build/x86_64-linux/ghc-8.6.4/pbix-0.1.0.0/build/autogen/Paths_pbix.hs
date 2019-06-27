{-# LANGUAGE CPP #-}
{-# LANGUAGE NoRebindableSyntax #-}
{-# OPTIONS_GHC -fno-warn-missing-import-lists #-}
module Paths_pbix (
    version,
    getBinDir, getLibDir, getDynLibDir, getDataDir, getLibexecDir,
    getDataFileName, getSysconfDir
  ) where

import qualified Control.Exception as Exception
import Data.Version (Version(..))
import System.Environment (getEnv)
import Prelude

#if defined(VERSION_base)

#if MIN_VERSION_base(4,0,0)
catchIO :: IO a -> (Exception.IOException -> IO a) -> IO a
#else
catchIO :: IO a -> (Exception.Exception -> IO a) -> IO a
#endif

#else
catchIO :: IO a -> (Exception.IOException -> IO a) -> IO a
#endif
catchIO = Exception.catch

version :: Version
version = Version [0,1,0,0] []
bindir, libdir, dynlibdir, datadir, libexecdir, sysconfdir :: FilePath

bindir     = "/home/barak/.cabal/bin"
libdir     = "/home/barak/.cabal/lib/x86_64-linux-ghc-8.6.4/pbix-0.1.0.0-inplace"
dynlibdir  = "/home/barak/.cabal/lib/x86_64-linux-ghc-8.6.4"
datadir    = "/home/barak/.cabal/share/x86_64-linux-ghc-8.6.4/pbix-0.1.0.0"
libexecdir = "/home/barak/.cabal/libexec/x86_64-linux-ghc-8.6.4/pbix-0.1.0.0"
sysconfdir = "/home/barak/.cabal/etc"

getBinDir, getLibDir, getDynLibDir, getDataDir, getLibexecDir, getSysconfDir :: IO FilePath
getBinDir = catchIO (getEnv "pbix_bindir") (\_ -> return bindir)
getLibDir = catchIO (getEnv "pbix_libdir") (\_ -> return libdir)
getDynLibDir = catchIO (getEnv "pbix_dynlibdir") (\_ -> return dynlibdir)
getDataDir = catchIO (getEnv "pbix_datadir") (\_ -> return datadir)
getLibexecDir = catchIO (getEnv "pbix_libexecdir") (\_ -> return libexecdir)
getSysconfDir = catchIO (getEnv "pbix_sysconfdir") (\_ -> return sysconfdir)

getDataFileName :: FilePath -> IO FilePath
getDataFileName name = do
  dir <- getDataDir
  return (dir ++ "/" ++ name)
