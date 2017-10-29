module Converters where

import           Control.Arrow
import           Control.Exception (Exception)
import           Data.Generics     (Typeable)
import           GHC.Generics      (Generic)
import           Text.Pandoc
import           Text.Parsec.Error
import           Text.Parsec.Pos   hiding (Line)

{-|
  convert markdown to html
  convert returns a either of pandoc error or html string
  convert gives pandoc error in case markdown parsing fails
-}
convert :: String -> Either PandocError (Meta, String)
convert markdownStr =
  fmap
    (meta &&& writeHtmlString (def {writerHtml5 = True, writerHighlight = True}))
    (readMarkdown (def {readerStandalone = True}) markdownStr)

meta :: Pandoc -> Meta
meta (Pandoc meta _) = meta

convertHandlingError :: String -> Either String (Meta, String)
convertHandlingError str =
  case convert str of
    Left err ->
      case err of
        ParseFailure string -> error string
        ParsecError input err' ->
          let errPos = errorPos err'
              errLine = sourceLine errPos
              errColumn = sourceColumn errPos
              ls = lines input ++ [""]
              errorInFile =
                if length ls > errLine - 1
                  then concat ["\n", ls !! (errLine - 1), "\n", replicate (errColumn - 1) ' ', "^"]
                  else ""
          in Left ("\nError at " ++ show err' ++ errorInFile)
    Right result -> Right result
