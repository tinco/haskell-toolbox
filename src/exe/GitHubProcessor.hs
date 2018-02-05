{-# LANGUAGE OverloadedStrings #-}
module Main where

import qualified Distribution.PackageDescription as Package
import qualified Distribution.Hackage.DB as DB
import qualified Data.Map as Map
import HaskellToolbox.Packages
import qualified Data.Vector as V
import qualified Data.Aeson as AE
import qualified Data.Maybe as M
import qualified Data.ByteString.Lazy as B
import qualified System.Directory as D
import Control.Monad

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

readGithubProjects = do
  filenames <- liftM (filter (\ f -> (take 5 f) == "repos")) $ D.listDirectory $ "data/"
  files <- mapM B.readFile filenames
  let parsedFiles = M.mapMaybe AE.decode' files
  return $ AE.Array $ V.concatMap (\(AE.Array v) -> v) (V.fromList parsedFiles)

filterOnCabal = undefined
