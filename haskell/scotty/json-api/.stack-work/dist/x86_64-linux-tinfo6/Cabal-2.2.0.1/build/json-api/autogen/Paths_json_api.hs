{-# LANGUAGE CPP #-}
{-# LANGUAGE NoRebindableSyntax #-}
{-# OPTIONS_GHC -fno-warn-missing-import-lists #-}
module Paths_json_api (
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

bindir     = "/home/chips/aulas/funcional/git/haskell/scotty/json-api/.stack-work/install/x86_64-linux-tinfo6/lts-12.11/8.4.3/bin"
libdir     = "/home/chips/aulas/funcional/git/haskell/scotty/json-api/.stack-work/install/x86_64-linux-tinfo6/lts-12.11/8.4.3/lib/x86_64-linux-ghc-8.4.3/json-api-0.1.0.0-Jr3dmeXt2Li1W6Rbh0B8n2-json-api"
dynlibdir  = "/home/chips/aulas/funcional/git/haskell/scotty/json-api/.stack-work/install/x86_64-linux-tinfo6/lts-12.11/8.4.3/lib/x86_64-linux-ghc-8.4.3"
datadir    = "/home/chips/aulas/funcional/git/haskell/scotty/json-api/.stack-work/install/x86_64-linux-tinfo6/lts-12.11/8.4.3/share/x86_64-linux-ghc-8.4.3/json-api-0.1.0.0"
libexecdir = "/home/chips/aulas/funcional/git/haskell/scotty/json-api/.stack-work/install/x86_64-linux-tinfo6/lts-12.11/8.4.3/libexec/x86_64-linux-ghc-8.4.3/json-api-0.1.0.0"
sysconfdir = "/home/chips/aulas/funcional/git/haskell/scotty/json-api/.stack-work/install/x86_64-linux-tinfo6/lts-12.11/8.4.3/etc"

getBinDir, getLibDir, getDynLibDir, getDataDir, getLibexecDir, getSysconfDir :: IO FilePath
getBinDir = catchIO (getEnv "json_api_bindir") (\_ -> return bindir)
getLibDir = catchIO (getEnv "json_api_libdir") (\_ -> return libdir)
getDynLibDir = catchIO (getEnv "json_api_dynlibdir") (\_ -> return dynlibdir)
getDataDir = catchIO (getEnv "json_api_datadir") (\_ -> return datadir)
getLibexecDir = catchIO (getEnv "json_api_libexecdir") (\_ -> return libexecdir)
getSysconfDir = catchIO (getEnv "json_api_sysconfdir") (\_ -> return sysconfdir)

getDataFileName :: FilePath -> IO FilePath
getDataFileName name = do
  dir <- getDataDir
  return (dir ++ "/" ++ name)
