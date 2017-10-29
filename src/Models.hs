module Models where

import           Data.Time
import           Text.Pandoc.Definition

newtype AbsoluteFilePath = AbsoluteFilePath
  { absoluteFilePath :: FilePath
  } deriving (Eq, Ord, Show)

newtype ValidFilePath = ValidFilePath
  { validFilePath :: FilePath
  } deriving (Eq, Ord, Show)

newtype DirFilePath = DirFilePath
  { dirFilePath :: FilePath
  } deriving (Eq, Ord, Show)

data BlogPostFileInfo = BlogPostFileInfo
  { srcFilePath  :: ValidFilePath
  , destFilePath :: ValidFilePath
  , fileMeta     :: Meta
  } deriving (Show)

data BlogPostInfo = BlogPostInfo
  { relativePostUrl :: String
  , title           :: String
  , date            :: String
  , summary         :: String
  , poster          :: Maybe String
  , tags            :: [String]
  } deriving (Show)

data PageType = Index | BlogPost

defaultBlogPostInfo :: BlogPostInfo
defaultBlogPostInfo =
  BlogPostInfo {relativePostUrl = "", date = "", title = "", summary = "", poster = Nothing, tags = []}
