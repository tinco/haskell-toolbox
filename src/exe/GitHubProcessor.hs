{-# LANGUAGE OverloadedStrings #-}
module Main where

import qualified Distribution.PackageDescription as Package
import qualified Distribution.Hackage.DB as DB
import qualified Data.Map as Map
import qualified Data.HashMap.Strict as HMap
import HaskellToolbox.Packages
import qualified Data.Vector as V
import qualified Data.Aeson as AE
import qualified Data.Maybe as M
import qualified Data.ByteString.Lazy as B
import qualified System.Directory as D
import Control.Monad

import Text.Regex.Posix

import Debug.Trace

type PackageDescriptions = Map.Map String Package.PackageDescription

data AnnotatedPackageDescription = AnnotatedPackageDescription {
    description :: Package.PackageDescription
}

main :: IO ()
main = do
  db <- readHackage
  let packages = buildPackageDescriptions db
  githubProjects <- readGithubProjects
  let githubProjectMap = makeGithubProjectsMap githubProjects
  -- let notOnCabal = filterOnCabal packages githubProjects
  return ()
  -- TODO annotate package descriptions with github info

makeGithubProjectsMap :: [AE.Value] -> Map.Map String AE.Value
makeGithubProjectsMap projects = foldl addProject Map.empty projects
  where
    addProject m p = Map.insert (getKey p) p m
    getKey (AE.Object o) = toKey $ M.fromJust $ HMap.lookup "repoHtmlUrl" o
    toKey (AE.String s) = show s

readGithubProjects = do
  filenames <- liftM (filter (\ f -> (take 5 f) == "repos")) $ D.listDirectory $ "data/"
  files <- mapM B.readFile filenames
  let parsedFiles = M.mapMaybe AE.decode' files
  return $ concatMap (\(AE.Array v) -> V.toList v) parsedFiles

filterOnCabal packages projects = M.mapMaybe isInPackages $ V.toList projects
  where
    isInPackages :: AE.Value -> Maybe AE.Value
    isInPackages p@(AE.Object o) = Just p
    isInPackages x = trace ("Something was not an object: " ++ (show x)) $ undefined
    -- githubRepoParser :: Parser (Maybe String)
    -- githubRepoParser = do
    --   CMC.skipManyTill MP.anyChar $ do
    --     MP.string' "github.com"
    --     MP.oneOf ['/',':']
    --     return True
    --   owner <- CMC.manyTill MP.anyChar (MP.char '/')
    --   repo <- CMC.manyTill MP.anyChar (MP.noneOf ['/', ' ', '?'])
    --   return $ Just $ owner ++ "/" ++ repo

    extractGithubRepo :: String -> String
    extractGithubRepo url = url =~ ("github\\.com[:/][^/]+/[^/ ?]+" :: String)
