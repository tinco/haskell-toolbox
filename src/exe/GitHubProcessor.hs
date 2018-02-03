{-# LANGUAGE OverloadedStrings #-}
module Main where

import qualified Distribution.PackageDescription as Package
import qualified Distribution.Hackage.DB as DB
import qualified Data.Map as Map
import HaskellToolbox.Packages

type PackageDescriptions = Map.Map String Package.PackageDescription

data AnnotatedPackageDescription = AnnotatedPackageDescription {
    description :: Package.PackageDescription
}

main :: IO ()
main = do
  db <- readHackage
  let packages = buildPackageDescriptions db
  githubProjects <- readGithubProjects
  let notOnCabal = filterOnCabal packages githubProjects
  return ()
  -- TODO annotate package descriptions with github info

readGithubProjects = undefined
filterOnCabal = undefined
