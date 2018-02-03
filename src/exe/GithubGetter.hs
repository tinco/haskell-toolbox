{-# LANGUAGE OverloadedStrings #-}

module Main where

import System.Environment (getEnv)

import qualified GitHub.Data as Github
import qualified GitHub.Request as Github
import qualified GitHub.Auth as Github

import Data.Text (Text, pack)
import qualified Data.Text.Encoding as TE

import qualified Data.Aeson as J
import qualified Data.ByteString.Lazy.Char8 as BL
import qualified Data.ByteString.Char8 as B

import Control.Concurrent (threadDelay)

import qualified Data.Vector as V

import Data.Time (getCurrentTime, formatTime, defaultTimeLocale, addUTCTime, nominalDay, parseTimeOrError)

instance J.ToJSON Github.Repo
instance J.ToJSON Github.RepoRef
instance J.ToJSON Github.SimpleOwner
instance J.ToJSON Github.OwnerType

main :: IO ()
main = do
  maybeToken <- getEnv "GITHUB_AUTH"
  case maybeToken of
    "" -> getRepositories  Nothing
    token -> getRepositories $ Just $ Github.OAuth $ B.pack token

getRepositories :: Maybe Github.Auth -> IO ()
getRepositories auth = do
    startTime <- getCurrentTime
    getRepositories'' startTime
  where
    endTime = parseTimeOrError True defaultTimeLocale "%Y-%-m-%-d" "2007-10-1"
    getRepositories'' date = do
      getRepositories' 1 date
      let nextDate = addUTCTime (-nominalDay) date
      currentTime <- getCurrentTime
      if nextDate > endTime
        then getRepositories'' nextDate
        else return ()

    getRepositories' page date = do
      let ts = formatTime defaultTimeLocale "%F" date
      let query = pack $ "language:haskell created:" ++ ts

      result <- searchRepos auth query page

      repos <- case result of
        Left e -> fail $ "Error: " ++ show e
        Right r -> return r

      let totalCount = Github.searchResultTotalCount repos
      let allRepos = Github.searchResultResults repos

      timeout <- case auth of
        Nothing -> return 6
        Just _ -> return 2

      threadDelay $ timeout * 1000000

      if null allRepos
        then return ()
        else do
          BL.writeFile ("data/repos-" ++ ts ++ "_" ++ (show page) ++ ".json") $ J.encode allRepos

          getRepositories' (page + 1) date

searchRepos :: Maybe Github.Auth -> Text -> Int -> IO (Either Github.Error (Github.SearchResult Github.Repo))
searchRepos auth s i = Github.executeRequestMaybe auth $ searchReposR s i

searchReposR :: Text -> Int -> Github.Request k (Github.SearchResult Github.Repo)
searchReposR searchString page = Github.query ["search", "repositories"] [
    ("q", Just $ TE.encodeUtf8 searchString),
    ("per_page", ( Just . TE.encodeUtf8 . pack ) $ show 100 ),
    ("page", ( Just . TE.encodeUtf8 . pack ) $ show page )
  ]
