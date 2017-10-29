module FileOperations where

import           Control.Arrow
import           Control.Monad
import           Data.List
import           Data.String.Utils
import           Models
import           System.Directory
import           System.FilePath

endsWith :: String -> String -> Bool
endsWith pattern str = endswith pattern str

copyDirWhen :: DirFilePath -> (ValidFilePath -> IO Bool) -> DirFilePath -> IO ()
copyDirWhen (DirFilePath src) predicate (DirFilePath dest) = do
  let cleanedSrcPath = dropTrailingPathSeparator src
  let cleanedDestPath = dropTrailingPathSeparator dest
  _ <- createDirectoryIfMissing True dest
  sourceFileNames <- listDirectory cleanedSrcPath
  let sourceTargetFilePathPairs = fmap ((</>) cleanedSrcPath &&& (</>) cleanedDestPath) sourceFileNames
  filesToBeCopied <- filterM (doesFileExist . fst) sourceTargetFilePathPairs
  let dirsToBeCopied = sourceTargetFilePathPairs \\ filesToBeCopied
  filtered <- filterM (predicate . ValidFilePath . fst) filesToBeCopied
  _ <- mapM_ (uncurry copyFile) filtered
  mapM_ (\(a, b) -> copyDirWhen (DirFilePath a) predicate (DirFilePath b)) dirsToBeCopied

copyDir :: DirFilePath -> DirFilePath -> IO ()
copyDir source = copyDirWhen source (return . const True)

markdownFilter :: ValidFilePath -> IO Bool
markdownFilter (ValidFilePath filePath) = do
  isFile <- doesFileExist filePath
  return $ isFile && (takeExtension filePath) == ".md"

copyMarkdownFiles :: DirFilePath -> DirFilePath -> IO ()
copyMarkdownFiles src dest = copyDirWhen src markdownFilter dest

expectMarkdownFilter :: ValidFilePath -> IO Bool
expectMarkdownFilter (ValidFilePath filePath) = do
  isFile <- doesFileExist filePath
  return
    (if isFile
       then takeExtension filePath /= ".md"
       else True)

copyEverythingExceptMarkdownFiles :: DirFilePath -> DirFilePath -> IO ()
copyEverythingExceptMarkdownFiles src dest = copyDirWhen src expectMarkdownFilter dest

appendDiff :: ValidFilePath -> ValidFilePath -> ValidFilePath -> ValidFilePath
appendDiff (ValidFilePath a) (ValidFilePath b) (ValidFilePath appendTo) =
  ValidFilePath $ appendTo </> takeDirectory (pathDiff a b)

pathDiff :: FilePath -> FilePath -> FilePath
pathDiff a b = helper (splitPath a) (splitPath b)
  where
    helper [] ys         = foldl1' combine ys
    helper xs []         = foldl1' combine xs
    helper (x:xs) (y:ys) = helper xs ys
