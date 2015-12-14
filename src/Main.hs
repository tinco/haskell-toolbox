{-# LANGUAGE OverloadedStrings #-} 
module Main where

import Packages
import qualified Data.Map as Map
import qualified Data.List as List
import Hakyll

main :: IO ()
main = do
  db <- readHackage
  generateMainPage

generateMainPage = hakyll $ do
  match "index.html" $ do
	route idRoute
	compile $ do
	  let indexContext =
		-- listField "posts" (postCtx tags) (return posts) <>
		-- field "tags" (\_ -> renderTagList tags) <>
		defaultContext

	  getResourceBody
		>>= applyAsTemplate indexContext
		>>= loadAndApplyTemplate "templates/content.html" indexContext
		>>= loadAndApplyTemplate "templates/default.html" indexContext
		>>= relativizeUrls


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

