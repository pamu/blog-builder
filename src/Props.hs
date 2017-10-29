{-# LANGUAGE QuasiQuotes #-}

module Props where

import           Text.RawString.QQ

cardColor :: String
cardColor = "indigo darken-4"

textColor :: String
textColor = "white-text"

tagTextColor :: String
tagTextColor = "indigo darken-4-text"

tagCSS :: String
tagCSS = [r|
.tag {
  font-size: 8px;
  background-color: white;
  border-radius: 1px;
  padding: 2px;
  margin: 4px;
}
|]
