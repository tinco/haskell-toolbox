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
	db <- DB.readHackage
	mapM_ putStrLn (getCategories db)

getCategories :: DB.Hackage -> [String]
getCategories db = Set.toList categories
	where
		packages = Map.elems db
		cabals = map (Package.packageDescription . List.last . Map.elems) packages
		categories = Set.fromList . concat . (map cleanCategories) $ cabals	
	
cleanCategories :: Package.PackageDescription -> [String]
cleanCategories package = (map (Text.unpack . Text.toTitle . Text.strip )) . (Text.splitOn ",") $ Text.pack . Package.category $ package
