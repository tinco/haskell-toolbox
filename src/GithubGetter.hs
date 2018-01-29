{-# LANGUAGE OverloadedStrings #-}

module Main where

import qualified GitHub.Data as Github
import qualified GitHub.Request as Github

import Data.Text (Text, pack)
import qualified Data.Text.Encoding as TE

import qualified Data.Aeson as J
import qualified Data.ByteString.Lazy.Char8 as BL

import Control.Concurrent (threadDelay)

instance J.ToJSON Github.Repo
instance J.ToJSON Github.RepoRef
instance J.ToJSON Github.SimpleOwner
instance J.ToJSON Github.OwnerType

main :: IO ()
main = getRepositories

getRepositories = getRepositories' 1
  where
    query = "language:haskell created:2017-01-20"
    auth = Nothing
    getRepositories' page = do
      result <- searchRepos auth query page

      repos <- case result of
        Left e -> fail $ "Error: " ++ show e
        Right r -> return r

      let totalCount = Github.searchResultTotalCount repos
      let allRepos = Github.searchResultResults repos

      BL.writeFile ("data/repos-" ++ (show page) ++ ".json") $ J.encode allRepos

      timeout <- case auth of
        Nothing -> return 6
        Just _ -> return 2

      threadDelay $ timeout * 1000000

      getRepositories' $ page + 1

searchRepos :: Maybe Github.Auth -> Text -> Int -> IO (Either Github.Error (Github.SearchResult Github.Repo))
searchRepos auth s i = Github.executeRequestMaybe auth $ searchReposR s i

searchReposR :: Text -> Int -> Github.Request k (Github.SearchResult Github.Repo)
searchReposR searchString page = Github.query ["search", "repositories"] [
    ("q", Just $ TE.encodeUtf8 searchString),
    ("per_page", ( Just . TE.encodeUtf8 . pack ) $ show 100 ),
    ("page", ( Just . TE.encodeUtf8 . pack ) $ show page )
  ]
