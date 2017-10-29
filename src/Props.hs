{-# LANGUAGE QuasiQuotes #-}

module Props where

import           Text.RawString.QQ

cardColor :: String
cardColor = "indigo darken-4"

textColor :: String
textColor = "blue-text"

highlightSyntax :: String
highlightSyntax = [r|
.tag {
  font-size: 8px;
  background-color: white;
  border-radius: 1px;
  padding: 2px;
  margin: 4px;
}
|]
