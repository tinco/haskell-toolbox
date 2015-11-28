{-# LANGUAGE OverloadedStrings #-} 
module Main where

import qualified Distribution.Hackage.DB as DB
import qualified Distribution.PackageDescription as Package
import qualified Data.Map as Map
import qualified Data.List as List
import qualified Data.Set as Set
import qualified Data.Text as Text

main :: IO ()
main = do
	putStrLn "Parsing Hackage DB.."
	db <- DB.readHackage
	putStrLn "Done."
	--putStrLn "Calculating categories.."
	--mapM_ putStrLn (getCategories db)
	--putStrLn "Done."
	putStrLn "Calculating dependency scores.."
	let dependantsCounts = buildDependantsCounts db
	let dependantStrings = Map.mapWithKey (\ n v -> n ++ " " ++ (show v)) dependantsCounts
	mapM_ putStrLn dependantStrings
	putStrLn "Done."

buildDependantsCounts :: DB.Hackage -> Map.Map String Int
buildDependantsCounts db = foldl insertDependant Map.empty dependencies
	where
		packages = Map.elems db
		cabals = map (Package.packageDescription . List.last . Map.elems) packages
		dependencies = map (\ (DB.Dependency n _) -> DB.unPackageName n) $ concatMap Package.buildDepends cabals
		insertDependant m d = Map.insertWith' (\ _ v -> v + 1) d 0 m

getCategories :: DB.Hackage -> [String]
getCategories db = Set.toList categories
	where
		packages = Map.elems db
		cabals = map (Package.packageDescription . List.last . Map.elems) packages
		categories = Set.fromList . concat . (map cleanCategories) $ cabals	
	
cleanCategories :: Package.PackageDescription -> [String]
cleanCategories package = (map (Text.unpack . Text.toTitle . Text.strip )) . (Text.splitOn ",") $ Text.pack . Package.category $ package
