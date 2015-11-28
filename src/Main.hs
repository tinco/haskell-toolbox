{-# LANGUAGE OverloadedStrings #-} 
module Main where

import qualified Distribution.Hackage.DB as DB
import qualified Distribution.PackageDescription as Package
import qualified Distribution.PackageDescription.Configuration as PackageConfiguration
import qualified Distribution.Package as Package (packageName)
import qualified Data.Map as Map
import qualified Data.List as List
import qualified Data.Set as Set
import qualified Data.Text as Text

import Debug.Trace

main :: IO ()
main = do
	putStrLn "Parsing Hackage DB.."
	db <- DB.readHackage
	putStrLn "Done."
	--putStrLn "Calculating categories.."
	--mapM_ putStrLn (getCategories db)
	--putStrLn "Done."

	let packageDescriptions = buildPackageDescriptions db

	putStrLn "Calculating dependency scores.."
	let dependantsCounts = buildDependantsCounts packageDescriptions
	putStrLn $ "Got dependantsCounts: " ++ (show dependantsCounts)
	let dependantStrings = Map.mapWithKey (\ n v -> n ++ " " ++ (show v)) dependantsCounts
	mapM_ putStrLn dependantStrings
	putStrLn "Done."

type PackageDescriptions = Map.Map String Package.PackageDescription

buildPackageDescriptions :: DB.Hackage -> PackageDescriptions 
buildPackageDescriptions db = foldl insert Map.empty $ map (resolve . List.last . Map.elems) $ Map.elems db 
	where
		insert m package = Map.insert (DB.unPackageName . Package.packageName $ package) package m
		resolve = PackageConfiguration.flattenPackageDescription

buildDependantsCounts :: PackageDescriptions -> Map.Map String Int
buildDependantsCounts db = foldl insertDependant Map.empty dependencies
	where
		packages = Map.elems db
		dependencies = map (\ (DB.Dependency n _) -> DB.unPackageName n) $ concatMap Package.buildDepends packages
		insertDependant m d = Map.insertWith' (\ _ v -> v + 1) d 0 m

getCategories :: DB.Hackage -> [String]
getCategories db = Set.toList categories
	where
		packages = Map.elems db
		cabals = map (Package.packageDescription . List.last . Map.elems) packages
		categories = Set.fromList . concat . (map cleanCategories) $ cabals	
	
cleanCategories :: Package.PackageDescription -> [String]
cleanCategories package = (map (Text.unpack . Text.toTitle . Text.strip )) . (Text.splitOn ",") $ Text.pack . Package.category $ package
