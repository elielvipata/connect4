{-# LANGUAGE CPP #-}
{-# LANGUAGE NoRebindableSyntax #-}
{-# OPTIONS_GHC -fno-warn-missing-import-lists #-}
module Paths_hw4 (
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

bindir     = "/Users/eliel/Downloads/hw4 - solutions/.stack-work/install/x86_64-osx/d3db1575f8728342abcd48f6366e7287884b08e8874138dfaa083cde264dfbf2/8.10.6/bin"
libdir     = "/Users/eliel/Downloads/hw4 - solutions/.stack-work/install/x86_64-osx/d3db1575f8728342abcd48f6366e7287884b08e8874138dfaa083cde264dfbf2/8.10.6/lib/x86_64-osx-ghc-8.10.6/hw4-0.1.0.0-AjRz4RahEqiAOpfy2wng70-hw4-test"
dynlibdir  = "/Users/eliel/Downloads/hw4 - solutions/.stack-work/install/x86_64-osx/d3db1575f8728342abcd48f6366e7287884b08e8874138dfaa083cde264dfbf2/8.10.6/lib/x86_64-osx-ghc-8.10.6"
datadir    = "/Users/eliel/Downloads/hw4 - solutions/.stack-work/install/x86_64-osx/d3db1575f8728342abcd48f6366e7287884b08e8874138dfaa083cde264dfbf2/8.10.6/share/x86_64-osx-ghc-8.10.6/hw4-0.1.0.0"
libexecdir = "/Users/eliel/Downloads/hw4 - solutions/.stack-work/install/x86_64-osx/d3db1575f8728342abcd48f6366e7287884b08e8874138dfaa083cde264dfbf2/8.10.6/libexec/x86_64-osx-ghc-8.10.6/hw4-0.1.0.0"
sysconfdir = "/Users/eliel/Downloads/hw4 - solutions/.stack-work/install/x86_64-osx/d3db1575f8728342abcd48f6366e7287884b08e8874138dfaa083cde264dfbf2/8.10.6/etc"

getBinDir, getLibDir, getDynLibDir, getDataDir, getLibexecDir, getSysconfDir :: IO FilePath
getBinDir = catchIO (getEnv "hw4_bindir") (\_ -> return bindir)
getLibDir = catchIO (getEnv "hw4_libdir") (\_ -> return libdir)
getDynLibDir = catchIO (getEnv "hw4_dynlibdir") (\_ -> return dynlibdir)
getDataDir = catchIO (getEnv "hw4_datadir") (\_ -> return datadir)
getLibexecDir = catchIO (getEnv "hw4_libexecdir") (\_ -> return libexecdir)
getSysconfDir = catchIO (getEnv "hw4_sysconfdir") (\_ -> return sysconfdir)

getDataFileName :: FilePath -> IO FilePath
getDataFileName name = do
  dir <- getDataDir
  return (dir ++ "/" ++ name)
