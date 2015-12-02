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

	putStrLn "Calculating dependency scores.."
	let dependantsCounts = buildDependantsCounts packageDescriptions
	putStrLn "Done."

	let categoryScores = buildCategoryScores packageDescriptions dependantsCounts
	putStrLn "ok"

	let categoryScoreLines = Map.mapWithKey (\ n v -> n ++ " " ++ (show v)) categoryScores
	mapM_ putStrLn categoryScoreLines

type PackageDescriptions = Map.Map String Package.PackageDescription
type ScoreMap = Map.Map String Int

buildPackageDescriptions :: DB.Hackage -> PackageDescriptions 
buildPackageDescriptions db = foldl insert Map.empty $ map (resolve . List.last . Map.elems) $ Map.elems db 
	where
		insert m package = Map.insert (DB.unPackageName . Package.packageName $ package) package m
		resolve = PackageConfiguration.flattenPackageDescription

buildCategoryScores :: PackageDescriptions -> ScoreMap -> ScoreMap
buildCategoryScores packageDescriptions packageScores = categoryScores
	where
		categoryScores = foldl increaseScores Map.empty $ Map.assocs packageScores
		increaseScores m (p, s) = foldl (increaseScore s) m $ Maybe.fromMaybe [] $ categories p
		categories p = do 
		  pkg <- Map.lookup p packageDescriptions
		  return $ cleanCategories pkg

buildDependantsCounts :: PackageDescriptions -> ScoreMap
buildDependantsCounts db = foldl insertDependant Map.empty dependencies
	where
		packages = Map.elems db
		dependencies = map (\ (DB.Dependency n _) -> DB.unPackageName n) $ concatMap Package.buildDepends packages
		insertDependant = increaseScore 1

increaseScore :: Int -> ScoreMap -> String -> ScoreMap
increaseScore i m k = Map.insertWith' (\ _ v -> v + i) k 0 m

getCategories :: DB.Hackage -> [String]
getCategories db = Set.toList categories
	where
		packages = Map.elems db
		cabals = map (Package.packageDescription . List.last . Map.elems) packages
		categories = Set.fromList . concat . (map cleanCategories) $ cabals	
	
cleanCategories :: Package.PackageDescription -> [String]
cleanCategories package = (map (Text.unpack . Text.toTitle . Text.strip )) . (Text.splitOn ",") $ Text.pack . Package.category $ package
