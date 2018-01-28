{-# LANGUAGE OverloadedStrings #-}

module Main where

import qualified GitHub.Data as Github
import qualified GitHub.Request as Github

import Data.Text (Text, pack)
import qualified Data.Text.Encoding as TE

import qualified Data.Aeson as J
import qualified Data.ByteString.Lazy.Char8 as BL

instance J.ToJSON Github.Repo
instance J.ToJSON Github.RepoRef
instance J.ToJSON Github.SimpleOwner
instance J.ToJSON Github.OwnerType

main :: IO ()
main = getRepositories

getRepositories = do
  result <- autoPaginateSearchRepos auth query

  repos <- case result of
    Left e -> fail $ "Error: " ++ show e
    Right r -> return r

  let totalCount = Github.searchResultTotalCount repos
  let allRepos = Github.searchResultResults repos

  BL.putStrLn $ J.encode allRepos

  where
    query = "language:haskell pushed:>2017-01-20"
    auth = Nothing

autoPaginateSearchRepos :: (Maybe Github.Auth) -> Text -> IO (Either Github.Error (Github.SearchResult Github.Repo))
autoPaginateSearchRepos auth searchString = do
  firstResult <- searchRepos auth searchString 1
  case firstResult of
    Left e -> return firstResult
    Right r -> return firstResult

searchRepos :: Maybe Github.Auth -> Text -> Int -> IO (Either Github.Error (Github.SearchResult Github.Repo))
searchRepos auth s i = Github.executeRequestMaybe auth $ searchReposR s i

searchReposR :: Text -> Int -> Github.Request k (Github.SearchResult Github.Repo)
searchReposR searchString page = Github.query ["search", "repositories"] [
    ("q", Just $ TE.encodeUtf8 searchString),
    ("per_page", ( Just . TE.encodeUtf8 . pack ) $ show 100 ),
    ("page", ( Just . TE.encodeUtf8 . pack ) $ show page )
  ]
