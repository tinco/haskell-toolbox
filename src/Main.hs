{-# LANGUAGE OverloadedStrings #-} 
module Main where

import Packages
import qualified Data.Map as Map
import qualified Data.List as List

import           Data.Monoid     ((<>))
import Hakyll
import Data.String
import Data.Ord

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
	generateMainPage cache

scoreSort :: (a, Int) -> (a, Int) -> Ordering
scoreSort a b = comparing (Down . snd) a b

relativeRoute = gsubRoute "pages/" (const "")

generateMainPage cache = hakyll $ do
	-- Read templates
	match "templates/*" $ compile templateCompiler

	match "pages/index.html" $ do
		route relativeRoute
		compile $ do
			let top20Categories = map (\ (n,i) -> Item (fromString n) (n,i) ) $ take 20 $ List.sortBy scoreSort $ Map.assocs $ categoryScores cache
			let top20Packages = map (\ (n,i) -> Item (fromString n) (n,i) ) $ take 20 $ List.sortBy scoreSort $ Map.assocs $ packageScores cache

			let indexContext =
				listField "top20categories" (topCategoryContext cache) (return top20Categories) <>
				listField "top20packages" (topPackageContext cache) (return top20Packages) <>
				defaultContext

			getResourceBody
				>>= applyAsTemplate indexContext
				>>= loadAndApplyTemplate "templates/content.html" indexContext
				>>= loadAndApplyTemplate "templates/default.html" indexContext
				>>= relativizeUrls

	mapM (buildCategoryPage cache) $ Map.keys $ categories cache

buildCategoryPage cache category = create [fromFilePath $ "categories/" ++ category ++ ".html"] $ do
	route idRoute
	compile $ do
		let coCategories = buildCoCategories (categories cache) [category]
		let scoredCoCategories = map (\ cc -> (cc, lookupScore cc $ categoryScores cache ) ) coCategories
		let topCoCategories = map (\ (n,i) -> Item (fromString n) (n,i) ) $ take 20 $ List.sortBy scoreSort $ scoredCoCategories
		--let topPackages = map (\ (n,i) -> Item (fromString n) (n,i) ) $ take 20 $ List.sortBy scoreSort $ Map.assocs $ packageScores cache

		let indexContext =
			constField "name" category <>
			listField "topCoCategories" (topCategoryContext cache) (return topCoCategories) <>
			--listField "top20packages" (topPackageContext cache) (return top20Packages) <>
			defaultContext

		makeItem ""
			>>= applyAsTemplate indexContext
			>>= loadAndApplyTemplate "templates/category.html" indexContext
			>>= loadAndApplyTemplate "templates/content.html" indexContext
			>>= loadAndApplyTemplate "templates/default.html" indexContext
			>>= relativizeUrls

topCategoryContext :: CachedOperations -> Context (String, Int)
topCategoryContext cache =
	field "name" (return.fst.itemBody) <>
	field "score" (return.show.snd.itemBody)

topPackageContext :: CachedOperations -> Context (String, Int)
topPackageContext = topCategoryContext 

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

