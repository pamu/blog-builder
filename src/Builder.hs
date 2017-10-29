module Builder where

import           Control.Monad          (filterM, join)
import           Converters
import           Data.String.Utils      (replace)
import           Models
import           Extractors
import           FileOperations
import           Holder
import           System.Directory       (createDirectoryIfMissing,
                                         doesDirectoryExist, doesFileExist,
                                         listDirectory)
import           System.FilePath        (dropTrailingPathSeparator,
                                         replaceExtension, takeExtension,
                                         takeFileName, (</>))
import           System.IO              (IOMode (..), hGetContents, hPutStrLn,
                                         withFile, withFile)
import           Text.Pandoc.Definition (Meta)

process :: (DirFilePath, DirFilePath) -> IO ()
process (src, dest) = do
  eitherInfoList <- processPostsDir src dest
  lists <-
    mapM
      (\eitherInfo ->
         case eitherInfo of
           Left str   -> putStrLn str >> return []
           Right info -> return [info])
      eitherInfoList
  let postInfoList = join lists
  _ <- createIndexFile postInfoList dest
  return ()

createIndexFile :: [BlogPostInfo] -> DirFilePath -> IO ()
createIndexFile postInfoList (DirFilePath filePath) =
  withFile (dropTrailingPathSeparator filePath </> "index.html") WriteMode $ \writeHandle -> do
    _ <- hPutStrLn writeHandle $ buildIndexFile postInfoList
    return ()

processPostsDir :: DirFilePath -> DirFilePath -> IO [Either String BlogPostInfo]
processPostsDir postsDir targetDir = do
  postDirs <- listDirPaths postsDir
  triplets <-
    mapM
      (\postDir -> do
         mdFiles <- listMDFiles postDir
         return $ fmap (\mdFile -> (postsDir, postDir, targetDir, mdFile)) mdFiles)
      postDirs
  processMarkdownFiles (join triplets)

listDirPaths :: DirFilePath -> IO [DirFilePath]
listDirPaths srcDir = do
  validFilePaths <- listPaths srcDir doesDirectoryExist
  return $ fmap (DirFilePath . validFilePath) validFilePaths

listPaths :: DirFilePath -> (FilePath -> IO Bool) -> IO [ValidFilePath]
listPaths (DirFilePath srcDir) predicate = do
  fileNames <- listDirectory srcDir
  let filePaths = fmap ((</>) $ dropTrailingPathSeparator srcDir) fileNames
  filtered <- filterM predicate filePaths
  return $ fmap ValidFilePath filtered

listMDFiles :: DirFilePath -> IO [ValidFilePath]
listMDFiles srcDir =
  listPaths
    srcDir
    (\filePath -> do
       exists <- doesFileExist filePath
       return $ exists && (takeExtension filePath == ".md"))

processMarkdownFiles :: [(DirFilePath, DirFilePath, DirFilePath, ValidFilePath)] -> IO [Either String BlogPostInfo]
processMarkdownFiles xs = helper xs [] 0
  where
    helper ::
         [(DirFilePath, DirFilePath, DirFilePath, ValidFilePath)]
      -> [Either String BlogPostInfo]
      -> Int
      -> IO [Either String BlogPostInfo]
    helper [] result counter =
      if counter == 0
        then return result
        else putStrLn "Processing markdown files completed!." >> return result
    helper (x:xs) result counter = processMarkdownFile x >>= (\info -> helper xs (info : result) (counter + 1))

processMarkdownFile :: (DirFilePath, DirFilePath, DirFilePath, ValidFilePath) -> IO (Either String BlogPostInfo)
processMarkdownFile (DirFilePath postsDir, DirFilePath markdownSrcDir, DirFilePath targetDir, ValidFilePath markdownFilePath) = do
  let fileName = takeFileName $ replaceExtension markdownFilePath "html"
  let newTargetDir = appendDiff (ValidFilePath postsDir) (ValidFilePath markdownFilePath) (ValidFilePath targetDir)
  let newFilePath = dropTrailingPathSeparator (validFilePath newTargetDir) </> fileName
  _ <- createDirectoryIfMissing True (validFilePath newTargetDir)
  result <- pipeOut (pathDiff targetDir newFilePath) (ValidFilePath markdownFilePath, ValidFilePath newFilePath)
  _ <- copyEverythingExceptMarkdownFiles (DirFilePath markdownSrcDir) (DirFilePath $ validFilePath newTargetDir)
  return result

pipeOut :: FilePath -> (ValidFilePath, ValidFilePath) -> IO (Either String BlogPostInfo)
pipeOut relativePath (ValidFilePath inPath, ValidFilePath outPath) =
  withFile outPath WriteMode $ \writeHandle ->
    withFile inPath ReadMode $ \readHandle -> do
      contents <- hGetContents readHandle
      case convertHandlingError contents of
        Right (meta, str) ->
          let eitherInfo = buildBlogPostInfo relativePath meta
          in case eitherInfo of
               Left err -> return $ Left $ "error processing file " ++ inPath ++ "error: " ++ err
               Right info -> hPutStrLn writeHandle (replace "$blogPost$" str (blogPostHtmlWrapper info)) >> return (Right info)
        Left err -> return $ Left ("error ocurred reading file: " ++ inPath ++ ", error: " ++ err)

fromMaybe :: b -> Maybe a -> Either b a
fromMaybe _ (Just a) = Right a
fromMaybe b Nothing  = Left b

buildBlogPostInfo :: FilePath -> Meta -> Either String BlogPostInfo
buildBlogPostInfo relativePath meta = do
  title <- fromMaybe "no title found" $ extractTitle meta
  summary <- fromMaybe "no summary found" $ extractSummary meta
  tags <- fromMaybe "no tags found" $ extractTags meta
  date <- fromMaybe "no date found" $ extractDate meta
  poster <- Right $ extractPoster meta
  return
    BlogPostInfo
    {relativePostUrl = relativePath, title = title, summary = summary, date = date, poster = poster, tags = tags}
