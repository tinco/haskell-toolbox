{-# LANGUAGE OverloadedStrings #-}
module HaskellToolbox.Packages where

import qualified Distribution.Hackage.DB as DB
import qualified Distribution.Package as P
import qualified Distribution.PackageDescription as Package
import qualified Distribution.PackageDescription.Configuration as PackageConfiguration
import qualified Distribution.Package as Package (packageName)
import qualified Data.Map as Map
import qualified Data.List as List
import qualified Data.Set as Set
import qualified Data.Text as Text
import qualified Data.Maybe as Maybe

import Debug.Trace

readHackage = DB.hackageTarball >>= DB.readTarball Nothing

type PackageDescriptions = Map.Map String Package.PackageDescription
type ScoreMap = Map.Map String Int
type Categories = Map.Map String [Package.PackageDescription]

packageName :: Package.PackageDescription -> String
packageName = show . Package.packageName

safeLast :: [a] -> Maybe a
safeLast [] = Nothing
safeLast l = Just $ List.last l

nullPackageDescription = Package.emptyPackageDescription -- this is annoying..

buildPackageDescriptions :: DB.HackageDB -> PackageDescriptions
buildPackageDescriptions db = foldl insert Map.empty $ getPackageDescriptions $ Map.elems db
	where
		insert m package = Map.insert (packageName $ package) package m
		getPackageDescriptions :: [DB.PackageData] -> [Package.PackageDescription]
		getPackageDescriptions packageData = map (resolve . safeLast . Map.elems) $ packageData
		resolve :: Maybe DB.VersionData -> Package.PackageDescription
		resolve (Just p) = PackageConfiguration.flattenPackageDescription . DB.cabalFile $ p
		resolve Nothing = nullPackageDescription

buildCategoryScores :: Categories -> ScoreMap -> ScoreMap
buildCategoryScores categories packageScores = categoryScores
	where
		categoryScores = foldl insertScores Map.empty $ Map.assocs categories
		insertScores m (c, ps) = Map.insert c (sum $ map lookupScore ps) m
		lookupScore p = Maybe.fromMaybe 0 (Map.lookup (packageName p) packageScores)

coCategoryPackages :: Categories -> [String] -> [Package.PackageDescription]
coCategoryPackages categories cocategories =
	List.foldl1' List.intersect $ map (\ n -> Maybe.fromMaybe [] (Map.lookup n categories)) cocategories

buildDependantsCounts :: [Package.PackageDescription] -> ScoreMap
buildDependantsCounts packages = foldl insertDependant Map.empty dependencies
	where
		dependencies = map (\ (P.Dependency n _) -> show n) $ concatMap Package.buildDepends packages
		insertDependant = increaseScore 1

buildSimilarPackages :: Categories -> [String] -> ScoreMap
buildSimilarPackages categories referenceCategories = packageScores
	where
		packageScores = foldl insertCategoryScores Map.empty referenceCategories
		insertCategoryScores m c = foldl insertPackageScores m $ lookupCategory c categories
		insertPackageScores m p = Map.insert (packageName p) ((lookupScore p m) + 1) m
		lookupScore p m = Maybe.fromMaybe 0 (Map.lookup (packageName p) m)

getCategories :: DB.HackageDB -> Categories
getCategories db = foldl insertPackage Map.empty packages
	where
		packages = Map.elems $ buildPackageDescriptions db
		insertPackage m package = foldl (insertPackage' package) m $ cleanCategories package
		insertPackage' p m category = Map.insertWith' (\_ v -> v ++ [p]) category [] m

buildCoCategories :: Categories -> [String] -> [String]
buildCoCategories categories subjects = cocategories List.\\ subjects
	where
		-- the intersect of packages that are in each subject category
		packages = coCategoryPackages categories subjects
		-- the union of the intersect of each of the packages with the subjects
		cocategories = foldl List.union [] $ map cleanCategories packages

cleanCategories :: Package.PackageDescription -> [String]
cleanCategories package = (map (Text.unpack . Text.toTitle . Text.strip )) . (Text.splitOn ",") $ Text.pack . Package.category $ package

increaseScore :: Int -> ScoreMap -> String -> ScoreMap
increaseScore i m k = Map.insertWith' (\ _ v -> v + i) k 0 m

lookupScore :: String -> ScoreMap -> Int
lookupScore n m = Maybe.fromMaybe 0 (Map.lookup n m)

lookupCategory :: String -> Categories -> [Package.PackageDescription]
lookupCategory n m = Maybe.fromMaybe [] (Map.lookup n m)

lookupPackage :: String -> PackageDescriptions -> Package.PackageDescription
lookupPackage n ps = Maybe.fromJust $ Map.lookup n ps
