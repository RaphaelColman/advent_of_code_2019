{-# LANGUAGE CPP #-}
{-# LANGUAGE NoRebindableSyntax #-}
{-# OPTIONS_GHC -fno-warn-missing-import-lists #-}
module Paths_AdventOfCode (
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

bindir     = "/home/raphael/haskell/advent_of_code/.stack-work/install/x86_64-linux/6c3a6373a5badc2a61f315873b21ef573a9d3715ff6630cba129ad82f58ed9b7/8.6.5/bin"
libdir     = "/home/raphael/haskell/advent_of_code/.stack-work/install/x86_64-linux/6c3a6373a5badc2a61f315873b21ef573a9d3715ff6630cba129ad82f58ed9b7/8.6.5/lib/x86_64-linux-ghc-8.6.5/AdventOfCode-0.1.0.0-6mLx1p012Y08l6RpOSPn1w"
dynlibdir  = "/home/raphael/haskell/advent_of_code/.stack-work/install/x86_64-linux/6c3a6373a5badc2a61f315873b21ef573a9d3715ff6630cba129ad82f58ed9b7/8.6.5/lib/x86_64-linux-ghc-8.6.5"
datadir    = "/home/raphael/haskell/advent_of_code/.stack-work/install/x86_64-linux/6c3a6373a5badc2a61f315873b21ef573a9d3715ff6630cba129ad82f58ed9b7/8.6.5/share/x86_64-linux-ghc-8.6.5/AdventOfCode-0.1.0.0"
libexecdir = "/home/raphael/haskell/advent_of_code/.stack-work/install/x86_64-linux/6c3a6373a5badc2a61f315873b21ef573a9d3715ff6630cba129ad82f58ed9b7/8.6.5/libexec/x86_64-linux-ghc-8.6.5/AdventOfCode-0.1.0.0"
sysconfdir = "/home/raphael/haskell/advent_of_code/.stack-work/install/x86_64-linux/6c3a6373a5badc2a61f315873b21ef573a9d3715ff6630cba129ad82f58ed9b7/8.6.5/etc"

getBinDir, getLibDir, getDynLibDir, getDataDir, getLibexecDir, getSysconfDir :: IO FilePath
getBinDir = catchIO (getEnv "AdventOfCode_bindir") (\_ -> return bindir)
getLibDir = catchIO (getEnv "AdventOfCode_libdir") (\_ -> return libdir)
getDynLibDir = catchIO (getEnv "AdventOfCode_dynlibdir") (\_ -> return dynlibdir)
getDataDir = catchIO (getEnv "AdventOfCode_datadir") (\_ -> return datadir)
getLibexecDir = catchIO (getEnv "AdventOfCode_libexecdir") (\_ -> return libexecdir)
getSysconfDir = catchIO (getEnv "AdventOfCode_sysconfdir") (\_ -> return sysconfdir)

getDataFileName :: FilePath -> IO FilePath
getDataFileName name = do
  dir <- getDataDir
  return (dir ++ "/" ++ name)
