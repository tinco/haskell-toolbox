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
import qualified Data.Maybe as Maybe

import Debug.Trace

main :: IO ()
main = do
	putStrLn "Parsing Hackage DB.."
	db <- DB.readHackage
	putStrLn "Done."
	let categories = getCategories db
	-- putStrLn "Calculating categories.."
	--mapM_ putStrLn (getCategories db)
	--putStrLn "Done."

	let packageDescriptions = buildPackageDescriptions db
	let dependantsCounts = buildDependantsCounts packageDescriptions
	let categoryScores = buildCategoryScores categories dependantsCounts

	let categoryScoreLines = Map.mapWithKey (\ n v -> n ++ " " ++ (show v)) categoryScores
	mapM_ putStrLn categoryScoreLines

type PackageDescriptions = Map.Map String Package.PackageDescription
type ScoreMap = Map.Map String Int
type Categories = Map.Map String [Package.PackageDescription]

packageName :: Package.PackageDescription -> String
packageName = DB.unPackageName . Package.packageName

buildPackageDescriptions :: DB.Hackage -> PackageDescriptions 
buildPackageDescriptions db = foldl insert Map.empty $ map (resolve . List.last . Map.elems) $ Map.elems db 
	where
		insert m package = Map.insert (packageName $ package) package m
		resolve = PackageConfiguration.flattenPackageDescription

buildCategoryScores :: Categories -> ScoreMap -> ScoreMap
buildCategoryScores categories packageScores = categoryScores
	where
		categoryScores = foldl insertScores Map.empty $ Map.assocs categories
		insertScores m (c, ps) = Map.insert c (sum $ map lookupScore ps) m
		lookupScore :: Package.PackageDescription -> Int
		lookupScore p = Maybe.fromMaybe 0 (Map.lookup (packageName p) packageScores)

buildDependantsCounts :: PackageDescriptions -> ScoreMap
buildDependantsCounts db = foldl insertDependant Map.empty dependencies
	where
		packages = Map.elems db
		dependencies = map (\ (DB.Dependency n _) -> DB.unPackageName n) $ concatMap Package.buildDepends packages
		insertDependant = increaseScore 1

increaseScore :: Int -> ScoreMap -> String -> ScoreMap
increaseScore i m k = Map.insertWith' (\ _ v -> v + i) k 0 m

getCategories :: DB.Hackage -> Categories
getCategories db = foldl insertPackage Map.empty packages
	where
		packages = map (Package.packageDescription . List.last . Map.elems) $ Map.elems db
		insertPackage m package = foldl (insertPackage' package) m $ cleanCategories package
		insertPackage' p m category = Map.insertWith' (\_ v -> v ++ [p]) category [] m
	
cleanCategories :: Package.PackageDescription -> [String]
cleanCategories package = (map (Text.unpack . Text.toTitle . Text.strip )) . (Text.splitOn ",") $ Text.pack . Package.category $ package
