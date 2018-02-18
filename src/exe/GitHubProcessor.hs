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

import qualified GitHub.Data as Github

import Text.Regex.Posix

import Debug.Trace

type AllPackageDescriptions = Map.Map String GithubAndHackagePackageDescription

data GithubAndHackagePackageDescription = GithubAndHackagePackageDescription {
    onHackage :: Bool,
    packageName :: String,
    maybeGithubName :: Maybe String,
    maybePackageDescription :: Maybe Package.PackageDescription,
    maybeGithubDescription :: Maybe Github.Repo
}

-- We want to show for each package how many github projects depend on it
-- that are not also on hackage.
-- So we want to have all github repos, then exclude the ones that we already have
-- the cabal file for. Then get the cabal file for all the ones we don't have
-- a cabal file for. Then for all the cabal files we get from github, we want
-- to discard all the projects with cabal files that identify a project that already
-- is on hackage. Then we want to discard all the projects that have less stars
-- than a project that we already have the cabal file of

main :: IO ()
main = do
  db <- readHackage
  let packages = buildPackageDescriptions db
  githubProjects <- readGithubProjects
  let githubProjectMap = makeGithubProjectsMap githubProjects
  -- let notOnCabal = filterOnCabal packages githubProjects
  return ()
  -- TODO annotate package descriptions with github info

readGithubProjects :: IO ([Github.Repo])
readGithubProjects = do
  filenames <- liftM (filter (\ f -> (take 5 f) == "repos")) $ D.listDirectory $ "data/"
  files <- mapM B.readFile filenames
  let parsedFiles = M.mapMaybe AE.decode' files :: [[Github.Repo]]
  return $ concat parsedFiles

makeGithubProjectsMap :: [Github.Repo] -> Map.Map String Github.Repo
makeGithubProjectsMap projects = foldl addProject Map.empty projects
  where
    addProject m p = maybeInsert (getKey p) p m
    maybeInsert k p m = (\ p'' -> Map.insert k p'' m) $ case Map.lookup k m of
      Just p' -> betterProject p p'
      Nothing -> p
    betterProject p p' = if starsP > starsP' then p else p'
      where
        starsP = Github.repoStargazersCount p
        starsP' = Github.repoStargazersCount p'
    getKey o = toKey $ Github.repoName o
    toKey s = show s

-- filterOnCabal packages projects = M.mapMaybe isInPackages $ V.toList projects
--   where
--     isInPackages :: Github.Repo -> Maybe AE.Value
--     isInPackages p@(AE.Object o) = head $ M.catMaybes $ getGithubRepos o
--     isInPackages x = trace ("Something was not an object: " ++ (show x)) $ undefined
    -- githubRepoParser :: Parser (Maybe String)
    -- githubRepoParser = do
    --   CMC.skipManyTill MP.anyChar $ do
    --     MP.string' "github.com"
    --     MP.oneOf ['/',':']
    --     return True
    --   owner <- CMC.manyTill MP.anyChar (MP.char '/')
    --   repo <- CMC.manyTill MP.anyChar (MP.noneOf ['/', ' ', '?'])
    --   return $ Just $ owner ++ "/" ++ repo

    extractGithubRepo :: String -> Maybe String
    extractGithubRepo url = maybeResult matchUrl
      where
        maybeResult [] = Nothing
        maybeResult (s:[]) = trace ("Got this hit: " ++ (show s)) $ Just $ head s
        maybeResult xs = trace ("Got more than one result: " ++ (show xs)) undefined
        matchUrl :: [[String]]
        matchUrl = url =~ ("github\\.com[:/][^/]+/[^/ ?]+" :: String)
