{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DisambiguateRecordFields #-}

-----------------------------------------------------------------------------

-- |
-- Module      :  Distribution.Simple.MHS
-- Copyright   :  Cabal Devs 2026
-- License     :  BSD3
--
-- Maintainer  :  cabal-devel@haskell.org
-- Portability :  portable
--
-- This module contains most of the MHS-specific code for configuring, building
-- and installing packages.

module Distribution.Simple.MHS
  ( configure
  , getInstalledPackages
  , buildLib
  , buildExe
  , installLib
  , registerPackage
  ) where

import Distribution.Compat.Prelude
import Prelude ()
import qualified Prelude as Prelude

import Distribution.InstalledPackageInfo
import Distribution.PackageDescription
import Distribution.Pretty
import Distribution.Simple.Compiler
import Distribution.Simple.LocalBuildInfo
import Distribution.Simple.Program
import Distribution.Simple.Utils
import Distribution.Utils.Path
import Distribution.Verbosity
import qualified Data.Map as Map (empty)
import Distribution.System (Platform)
import Distribution.Version (mkVersion, orLaterVersion)
import Language.Haskell.Extension
import Distribution.Simple.PackageIndex (InstalledPackageIndex)
import qualified Distribution.Simple.PackageIndex as PackageIndex

import Distribution.Types.PackageName (mkPackageName)
import Distribution.Types.PackageId (PackageIdentifier(..))
import Distribution.Types.UnitId (mkUnitId)
import Distribution.Types.ComponentId (mkComponentId)
import Distribution.ModuleName (fromString)
import Data.Char (isSpace)
import Data.Maybe (fromMaybe, listToMaybe, mapMaybe)

-- -----------------------------------------------------------------------------
-- Configuring

configure
  :: Verbosity
  -> Maybe FilePath
  -> ProgramDb
  -> IO (Compiler, Maybe Platform, ProgramDb)
configure verbosity hcPath progdb = do
  (_mhsProg, mhsVersion, progdb') <-
    requireProgramVersion
      verbosity
      mhsProgram
      (orLaterVersion (mkVersion [0, 10, 0]))
      (userMaybeSpecifyPath "mhs" hcPath progdb)

  let comp =
        Compiler
          { compilerId = CompilerId MHS mhsVersion
          , compilerAbiTag = NoAbiTag
          , compilerCompat = []
          , compilerLanguages = mhsLanguages
          , compilerExtensions = mhsLanguageExtensions
          , compilerProperties = Map.empty
          , compilerWiredInUnitIds = Nothing
          }
      compPlatform = Nothing
  info verbosity $ "MHS configured with languages: " ++ show (map fst mhsLanguages)
  return (comp, compPlatform, progdb')

mhsLanguages :: [(Language, CompilerFlag)]
mhsLanguages = [(Haskell2010, ""), (Haskell98, "")] -- MHS docs say it supports 2010 but 98 should be fine too

mhsLanguageExtensions :: [(Extension, Maybe CompilerFlag)]
mhsLanguageExtensions =
  [
  -- TODO: use the list from the MHS documentation
  ]

getInstalledPackages
  :: Verbosity
  -> Compiler
  -> Maybe (SymbolicPath CWD (Dir from))
  -> PackageDBStackX (SymbolicPath from (Dir PkgDB))
  -> ProgramDb
  -> IO InstalledPackageIndex
getInstalledPackages verbosity _comp _mbWorkDir _packagedbs progdb = do
  let mhs = fromMaybe (error "mhs not configured") $ lookupProgram mhsProgram progdb
  output <- getProgramOutput verbosity mhs ["-L"]
  let pkgs = parseListOutput output
  warn verbosity $ "MHS packages found: " ++ show pkgs
  ipkgs <- traverse (getPackageInfo verbosity progdb) pkgs
  warn verbosity $ "MHS installed packages: " ++ show (map installedUnitId ipkgs)
  let idx = PackageIndex.fromList ipkgs
  warn verbosity $ "Index lookup 'base': " ++ show (PackageIndex.lookupUnitId idx (mkUnitId "base"))
  return idx

parseListOutput :: String -> [String]
parseListOutput out =
  [ pkgname
  | l <- lines out
  , "  " `isPrefixOf` l
  , let nameVer = drop 2 l
  , Just pkgname <- [parseNameVer nameVer]
  ]
  where
    parseNameVer :: String -> Maybe String
    parseNameVer s = Just s -- Use the full string "pkg-ver" for querying

getPackageInfo :: Verbosity -> ProgramDb -> String -> IO InstalledPackageInfo
getPackageInfo verbosity progdb pkgNameVer = do
   let mhs = fromMaybe (error "mhs not configured") $ lookupProgram mhsProgram progdb
   out <- getProgramOutput verbosity mhs ["-L" ++ pkgNameVer]
   let fields = parseFields out
       nameStr = fromMaybe (error $ "no name in " ++ pkgNameVer) $ lookup "name" fields >>= listToMaybe
       verStr = fromMaybe (error $ "no version in " ++ pkgNameVer) $ lookup "version" fields >>= listToMaybe
       version = mkVersion $ map Prelude.read $ split (== '.') verStr
       pkgName = mkPackageName nameStr
       pkgId = PackageIdentifier pkgName version
       depends = fromMaybe [] $ lookup "depends" fields
       exposed = fromMaybe [] $ lookup "exposed-modules" fields
       hidden = fromMaybe [] $ lookup "other-modules" fields
       
       compid = mkComponentId nameStr
       unitid = mkUnitId nameStr
       
   return emptyInstalledPackageInfo
     { sourcePackageId = pkgId
     , installedUnitId = unitid
     , installedComponentId_ = compid
     , depends = [ mkUnitId d | d <- depends, not (null d) ] 
     , exposedModules = map (exposedModule . fromString) exposed
     , hiddenModules = map fromString hidden
     , exposed = True
     }
  where
    split :: (Char -> Bool) -> String -> [String]
    split p s =  case dropWhile p s of
      "" -> []
      s' -> w : split p s''
            where (w, s'') = break p s'

    exposedModule m = ExposedModule m Nothing

parseFields :: String -> [(String, [String])]
parseFields input = go (lines input) []
  where
    go [] acc = reverse acc
    go (l:ls) acc
      | null l = go ls acc
      | isSpace (l !! 0) = 
          case acc of
            ((k, vals):rest) -> go ls ((k, vals ++ [trim l]):rest)
            [] -> go ls acc
      | otherwise = 
          let (k, v) = break (== ':') l
              key = k
              val = drop 1 v 
              val' = trim val
              vals = if null val' then [] else [val']
          in go ls ((key, vals):acc)
          
    trim = f . f
      where f = reverse . dropWhile isSpace

-- -----------------------------------------------------------------------------
-- Building

buildLib
  :: Verbosity
  -> PackageDescription
  -> LocalBuildInfo
  -> Library
  -> ComponentLocalBuildInfo
  -> IO ()
buildLib _verbosity _pkg_descr _lbi _lib _clbi = return ()

buildExe
  :: Verbosity
  -> PackageDescription
  -> LocalBuildInfo
  -> Executable
  -> ComponentLocalBuildInfo
  -> IO ()
buildExe verbosity _pkg_descr lbi exe _clbi = do
  let runMhsProg = runDbProgramCwd verbosity (mbWorkDirLBI lbi) mhsProgram (withPrograms lbi)
  
  let mbWorkDir = mbWorkDirLBI lbi
  srcMainPath <- findFileCwd verbosity mbWorkDir (hsSourceDirs $ buildInfo exe) (modulePath exe)
  
  let odir = buildDir lbi
      u = interpretSymbolicPathCWD
      
      mhsArgs = 
           ["-i" ++ u d | d <- hsSourceDirs (buildInfo exe)]
        ++ ["-o", u $ odir </> makeRelativePathEx (prettyShow (exeName exe))]
        ++ [u srcMainPath]

  runMhsProg mhsArgs

-- -----------------------------------------------------------------------------
-- Installation

installLib
  :: Verbosity
  -> LocalBuildInfo
  -> FilePath
  -> FilePath
  -> FilePath
  -> PackageDescription
  -> Library
  -> ComponentLocalBuildInfo
  -> IO ()
installLib _verbosity _lbi _targetDir _dynlibTargetDir _builtDir _pkg _library _clbi = return ()

-- -----------------------------------------------------------------------------
-- Registering

registerPackage
  :: Verbosity
  -> Maybe (SymbolicPath CWD (Dir from))
  -> Compiler
  -> ProgramDb
  -> PackageDBStackS from
  -> InstalledPackageInfo
  -> IO ()
registerPackage _verbosity _mbWorkDir _comp _progdb _packageDbs _installedPkgInfo = return ()
