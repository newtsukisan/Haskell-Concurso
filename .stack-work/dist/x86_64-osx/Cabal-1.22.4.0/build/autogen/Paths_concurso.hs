module Paths_concurso (
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

bindir     = "/Users/trabajo/Documents/Cursos/FP/proyectos/concurso/.stack-work/install/x86_64-osx/lts-3.11/7.10.2/bin"
libdir     = "/Users/trabajo/Documents/Cursos/FP/proyectos/concurso/.stack-work/install/x86_64-osx/lts-3.11/7.10.2/lib/x86_64-osx-ghc-7.10.2/concurso-0.1.0.0-9AEwiLQ4KB34fA4pkFzeil"
datadir    = "/Users/trabajo/Documents/Cursos/FP/proyectos/concurso/.stack-work/install/x86_64-osx/lts-3.11/7.10.2/share/x86_64-osx-ghc-7.10.2/concurso-0.1.0.0"
libexecdir = "/Users/trabajo/Documents/Cursos/FP/proyectos/concurso/.stack-work/install/x86_64-osx/lts-3.11/7.10.2/libexec"
sysconfdir = "/Users/trabajo/Documents/Cursos/FP/proyectos/concurso/.stack-work/install/x86_64-osx/lts-3.11/7.10.2/etc"

getBinDir, getLibDir, getDataDir, getLibexecDir, getSysconfDir :: IO FilePath
getBinDir = catchIO (getEnv "concurso_bindir") (\_ -> return bindir)
getLibDir = catchIO (getEnv "concurso_libdir") (\_ -> return libdir)
getDataDir = catchIO (getEnv "concurso_datadir") (\_ -> return datadir)
getLibexecDir = catchIO (getEnv "concurso_libexecdir") (\_ -> return libexecdir)
getSysconfDir = catchIO (getEnv "concurso_sysconfdir") (\_ -> return sysconfdir)

getDataFileName :: FilePath -> IO FilePath
getDataFileName name = do
  dir <- getDataDir
  return (dir ++ "/" ++ name)
