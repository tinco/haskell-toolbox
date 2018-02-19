{-# LANGUAGE OverloadedStrings, ScopedTypeVariables #-}
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
import Data.Monoid

import qualified GitHub.Data as Github

import Text.Regex.Posix

import Debug.Trace

type AllPackageDescriptions = Map.Map String GithubAndHackagePackageDescription

type GithubRepos = Map.Map String Github.Repo

data GithubAndHackagePackageDescription = GithubAndHackagePackageDescription {
    onHackage :: !Bool,
    packageName :: !String,
    maybeGithubName :: !(Maybe String),
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
  -- makeMergedDescriptions will try and match github projects to hackage cabal files
  let mergedProjects = makeMergedDescriptions packages githubProjectMap
  -- After they are merged we still need to download the missing cabal files.
  -- After downloading missing cabal files we should run another deduplication step
  -- so any github projects with repo names differing from their cabalfile names
  -- are renamed to their cabalfile name, and merged with a hackage entry if existing
  return ()
  -- TODO annotate package descriptions with github info

readGithubProjects :: IO ([Github.Repo])
readGithubProjects = do
  filenames <- liftM (filter (\ f -> (take 5 f) == "repos")) $ D.listDirectory $ "data/"
  files <- mapM B.readFile filenames
  let parsedFiles = M.mapMaybe AE.decode' files :: [[Github.Repo]]
  return $ concat parsedFiles

makeGithubProjectsMap :: [Github.Repo] -> GithubRepos
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

-- We want to check for each github repo if it is related to a cabal project
-- if it is not we want to keep it.
-- Most performant would be to go through all cabal packages, and construct
-- a github project name, and then removing the resulting list of project names
-- from our project list.
--
-- The resulting AllPackageDescriptions could still have a bunch of github projects
-- that are not linked to their cabal description. Checking their cabal files is
-- still required to get a completely accurate AllPackageDescriptions.
makeMergedDescriptions :: PackageDescriptions -> GithubRepos -> AllPackageDescriptions
makeMergedDescriptions packages projects = makeMergedDescriptions' packages projects Map.empty
makeMergedDescriptions' packages projects allDescriptions = discoverGithubCabalRelations $ matchProjectNames
  -- we can do this in multiple steps.
  -- 1. check if the cabal project name is directly a github repo name, if it is
  --    then add to allDescriptions, remove from packages and projects and recurses
  -- 2. check if we can extract the github repo from the cabalfile properties, if
  --    we can then add to allDescriptions, remove from packages and projects and recurse
  --
  where
    discoverGithubCabalRelations descriptions = Map.map discoverGithubCabalRelation descriptions
      where
        discoverGithubCabalRelation description
          | M.isJust $ maybeGithubName description = description
          | M.isJust discoveredGithubProject = updatedDescription
          | otherwise = description
          where
            package = M.fromJust $ maybePackageDescription description
            discoveredGithubProject :: Maybe Github.Repo
            discoveredGithubProject = do
              name <- findGithubName
              Map.lookup name projects

            findGithubName :: Maybe String
            findGithubName = getFirst $ do
              let homepage :: Maybe String = extractGithubRepo $ Package.homepage package
              let repoLocations :: [Maybe String] = map extractGithubRepo $ M.mapMaybe Package.repoLocation $ Package.sourceRepos package
              mconcat $ fmap First (homepage : repoLocations)
              -- TODO other possible locations of github url?

            updatedDescription = description {
              maybeGithubName = liftM (show.Github.repoName) discoveredGithubProject,
              maybeGithubDescription = discoveredGithubProject
            }

    matchProjectNames = Map.map createGithubAndHackagePackageDescription packages
      where
        createGithubAndHackagePackageDescription p = GithubAndHackagePackageDescription {
          onHackage = True,
          Main.packageName = name,
          maybeGithubName = githubName,
          maybePackageDescription = Just p,
          maybeGithubDescription = githubRepo
        }
          where
            name = HaskellToolbox.Packages.packageName p
            githubRepo = Map.lookup name projects
            githubName = liftM (show.Github.repoName) githubRepo

    extractGithubRepo :: String -> Maybe String
    extractGithubRepo url = maybeResult matchUrl
      where
        maybeResult [] = Nothing
        maybeResult (s:[]) = trace ("Got this hit: " ++ (show s)) $ Just $ head s
        maybeResult xs = trace ("Got more than one result: " ++ (show xs)) undefined
        matchUrl :: [[String]]
        matchUrl = url =~ ("github\\.com[:/][^/]+/[^/ ?]+" :: String)

            -- We replaced the following repoParser with a regex:
            -- githubRepoParser :: Parser (Maybe String)
            -- githubRepoParser = do
            --   CMC.skipManyTill MP.anyChar $ do
            --     MP.string' "github.com"
            --     MP.oneOf ['/',':']
            --     return True
            --   owner <- CMC.manyTill MP.anyChar (MP.char '/')
            --   repo <- CMC.manyTill MP.anyChar (MP.noneOf ['/', ' ', '?'])
            --   return $ Just $ owner ++ "/" ++ repo
