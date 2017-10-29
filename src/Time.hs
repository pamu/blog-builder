{-# LANGUAGE ScopedTypeVariables #-}

module Time where

import           Control.Exception
import           Data.Time

parseTime :: String -> IO (Maybe UTCTime)
parseTime str = do
  (value :: Either IOException UTCTime) <-
    try $ return $ parseTimeOrError True defaultTimeLocale "%d-%m-%Y" str
  case value of
    Right x -> return $ Just x
    _       -> return Nothing
