module Validation where

import           Builder                    (process)
import           Control.Monad.IO.Class
import           Control.Monad.Trans.Either
import           Data.List                  (isPrefixOf)
import           Data.Maybe                 (fromJust)
import           System.Directory           (doesDirectoryExist,
                                             getHomeDirectory)
import           System.FilePath            (addTrailingPathSeparator, isValid,
                                             normalise)
import           System.Path.NameManip      (absolute_path, guess_dotdot)

import           Control.Monad
import           Models

{-| converts relative path into absolute path
   for example : ~/foo/bar will be converted to /home/user/foo/bar
   ../foo/bar will be converted to /somePath/foo/bar
-}
absolutize :: String -> IO AbsoluteFilePath
absolutize aPath
  | "~" `isPrefixOf` aPath = do
    homePath <- getHomeDirectory
    return $ AbsoluteFilePath $ normalise $ addTrailingPathSeparator homePath ++ tail aPath
  | otherwise = do
    pathMaybeWithDots <- absolute_path aPath
    return $ AbsoluteFilePath $ fromJust $ guess_dotdot pathMaybeWithDots

checkValidity :: AbsoluteFilePath -> Either String ValidFilePath
checkValidity (AbsoluteFilePath filePath) =
  if isValid filePath
    then Right $ ValidFilePath filePath
    else Left (show filePath ++ " is not valid.")

checkIsDir :: ValidFilePath -> EitherT String IO DirFilePath
checkIsDir (ValidFilePath filePath) =
  EitherT $
  doesDirectoryExist filePath >>= \result ->
    if result
      then return $ Right $ DirFilePath filePath
      else return $ Left (show filePath ++ " is not dir.")

usage :: String
usage = "Usage: <program-name> <input-folder> <output-folder>"

takeFirstTwo :: [String] -> Either String (String, String)
takeFirstTwo (x:y:_) = Right (x, y)
takeFirstTwo _       = Left usage

validate :: [String] -> EitherT String IO (DirFilePath, DirFilePath)
validate args = do
  (a, b) <- hoistEither $ takeFirstTwo args
  aDir <- validateInputFilePath a
  bDir <- validateInputFilePath b
  return (aDir, bDir)

validateInputFilePath :: String -> EitherT String IO DirFilePath
validateInputFilePath str = do
  absPath <- liftIO $ absolutize str
  validityCheck <- hoistEither $ checkValidity absPath
  checkIsDir validityCheck

validateAndProcess :: [String] -> IO ()
validateAndProcess args = do
  input <- runEitherT $ validate args
  case input of
    Right input -> Control.Monad.void (process input)
    Left msg    -> putStrLn msg
