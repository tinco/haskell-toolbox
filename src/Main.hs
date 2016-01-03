{-# LANGUAGE OverloadedStrings #-} 
module Main where

import Packages
import qualified Data.Map as Map
import qualified Data.List as List
import qualified Data.Char as Char

import           Data.Monoid     ((<>))
import Hakyll
import Data.String
import Data.Ord
import Debug.Trace

data CachedOperations = CachedOperations {
	categories :: !Categories,
	packageDescriptions :: !PackageDescriptions,
	packageScores :: !ScoreMap,
	categoryScores :: !ScoreMap
}

main :: IO ()
main = do
	db <- readHackage
	let packageDescriptions = buildPackageDescriptions db
	let categories = getCategories db
	let packageScores = buildDependantsCounts $ Map.elems packageDescriptions
	let categoryScores = buildCategoryScores categories packageScores
	let cache = CachedOperations {
		categories = categories,
		packageDescriptions = packageDescriptions,
		packageScores = packageScores,
		categoryScores = categoryScores
	}

	generatePages cache

scoreSort :: (a, Int) -> (a, Int) -> Ordering
scoreSort a b = comparing (Down . snd) a b

scoreToItem :: (String, Int) -> Item (String, Int)
scoreToItem (n,i) = Item (fromString n) (n,i)

stringToItem :: String -> Item (String)
stringToItem s = Item (fromString s) s

relativeRoute = gsubRoute "pages/" (const "")

generatePages cache = hakyll $ do
	-- Read templates
	match "templates/*" $ compile templateCompiler

	generateMainPage cache
	mapM (buildIndexPage cache) $ ['a'..'z']
	--mapM (buildPackagePage cache) $ Map.keys $ packageDescriptions cache
	--mapM (buildCategoryPage cache) $ Map.keys $ categories cache

generateMainPage cache = match "pages/index.html" $ do
	route relativeRoute
	compile $ do
		let top20Categories = map scoreToItem $ take 20 $ List.sortBy scoreSort $ Map.assocs $ categoryScores cache
		let top20Packages = map scoreToItem $ take 20 $ List.sortBy scoreSort $ Map.assocs $ packageScores cache

		let indexContext =
			listField "top20categories" (topCategoryContext cache) (return top20Categories) <>
			listField "top20packages" (topPackageContext cache) (return top20Packages) <>
			defaultContext

		getResourceBody
			>>= applyAsTemplate indexContext
			>>= loadAndApplyTemplate "templates/content.html" indexContext
			>>= loadAndApplyTemplate "templates/default.html" indexContext
			>>= relativizeUrls


buildCategoryPage cache category = create [fromFilePath $ "categories/" ++ category ++ ".html"] $ do
	route idRoute
	compile $ do
		let coCategories = buildCoCategories (categories cache) [category]
		let scoredCoCategories = map (\ cc -> (cc, lookupScore cc $ categoryScores cache ) ) coCategories
		let topCoCategories = map (\ (n,i) -> Item (fromString n) (n,i) ) $ take 20 $ List.sortBy scoreSort $ scoredCoCategories
		let packages = map packageName $ lookupCategory category (categories cache) 
		let scoredPackages = map (\ p -> (p, lookupScore p $ packageScores cache ) ) packages
		let topPackages = map (\ (n,i) -> Item (fromString n) (n,i) ) $ take 20 $ List.sortBy scoreSort $ scoredPackages

		let indexContext =
			constField "name" category <>
			listField "topCoCategories" (topCategoryContext cache) (return topCoCategories) <>
			listField "topPackages" (topPackageContext cache) (return topPackages) <>
			defaultContext

		makeItem ""
			>>= applyAsTemplate indexContext
			>>= loadAndApplyTemplate "templates/category.html" indexContext
			>>= loadAndApplyTemplate "templates/content.html" indexContext
			>>= loadAndApplyTemplate "templates/default.html" indexContext
			>>= relativizeUrls

buildPackagePage cache package = create [fromFilePath $ "packages/" ++ package ++ ".html"] $ do
	route idRoute
	compile $ do
		let packageDescription = lookupPackage package (packageDescriptions cache)
		let packageCategories = cleanCategories packageDescription
		-- TODO we need to reject dissimilar packages
		let mostSimilarPackages = map fst $ List.sortBy scoreSort $ Map.assocs $ buildSimilarPackages (categories cache) packageCategories
		let topSimilarPackages = map scoreToItem $ take 20 $ List.sortBy scoreSort $ map (\ n -> (n, lookupScore n $ packageScores cache) ) mostSimilarPackages

		let indexContext =
			constField "name" package <>
			listField "similarPackages" (topPackageContext cache) (return topSimilarPackages) <>
			listField "categories" (categoryContext cache) (return $ map stringToItem packageCategories) <>
			defaultContext

		makeItem ""
			>>= applyAsTemplate indexContext
			>>= loadAndApplyTemplate "templates/package.html" indexContext
			>>= loadAndApplyTemplate "templates/content.html" indexContext
			>>= loadAndApplyTemplate "templates/default.html" indexContext
			>>= relativizeUrls

buildIndexPage cache letter = create [fromFilePath $ "indexes/" ++ [letter] ++ ".json"] $ do
	route idRoute
	compile $ do
		let letterFilter ((l:ls), i) = l == letter || l == (Char.toUpper letter)
		    letterFilter ((ls), i) = False
		let packages = map scoreToItem $ filter letterFilter $ Map.assocs $ packageScores cache
		let categories = map scoreToItem $ filter letterFilter $ Map.assocs $ categoryScores cache

		let indexContext =
			constField "letter" ([letter]) <>
			listField "packages" (topPackageContext cache) (return packages) <>
			listField "categories" (topCategoryContext cache) (return categories) <>
			defaultContext

		makeItem ""
			>>= loadAndApplyTemplate "templates/index.json" indexContext
			>>= relativizeUrls

topCategoryContext :: CachedOperations -> Context (String, Int)
topCategoryContext cache =
	field "name" (return.fst.itemBody) <>
	field "score" (return.show.snd.itemBody)

topPackageContext :: CachedOperations -> Context (String, Int)
topPackageContext = topCategoryContext 

categoryContext :: CachedOperations -> Context (String)
categoryContext cache = field "name" (return.itemBody)

debugPackages :: IO ()
debugPackages = do
	putStrLn "Parsing Hackage DB.."
	db <- readHackage
	putStrLn "Done."
	let categories = getCategories db
	-- putStrLn "Calculating categories.."
	--mapM_ putStrLn (getCategories db)
	--putStrLn "Done."

	let packageDescriptions = buildPackageDescriptions db
	let dependantsCounts = buildDependantsCounts $ Map.elems packageDescriptions
	let categoryScores = buildCategoryScores categories dependantsCounts
	-- let categoryScoreLines = Map.mapWithKey (\ n v -> n ++ " " ++ (show v)) categoryScores
	-- mapM_ putStrLn categoryScoreLines
	let coCategoryString c = c ++ ": " ++ (List.intercalate ", " $ buildCoCategories categories [c])
	let coCategories = map coCategoryString $ Map.keys categories
	mapM_ putStrLn coCategories

