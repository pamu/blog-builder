{-# LANGUAGE ExtendedDefaultRules #-}
{-# LANGUAGE OverloadedStrings    #-}

module Holder where

import           Data.Char
import           Data.Foldable
import qualified Data.Text      as T
import qualified Data.Text.Lazy as LT
import           Models
import           Lucid.Base
import           Lucid.Html5
import           Props

jqueryJS :: Html ()
jqueryJS = script_ [type_ "text/javascript", src_ "https://code.jquery.com/jquery-3.2.1.min.js"] ""

materializeJS :: Html ()
materializeJS = script_ [type_ "text/javascript", src_ "https://code.jquery.com/jquery-3.2.1.min.js"] ""

meta :: Html ()
meta = meta_ [name_ "viewport", content_ "width=device-width, intial-scale=1.0"]

fonts :: Html ()
fonts = link_ [ rel_ "stylesheet", href_ "https://cdnjs.cloudflare.com/ajax/libs/materialize/0.100.2/js/materialize.min.js"]

css :: Html ()
css = link_ [rel_ "stylesheet", href_ "https://cdnjs.cloudflare.com/ajax/libs/materialize/0.100.2/css/materialize.min.css"]

mainContent :: Html () -> Html ()
mainContent content =
  html_ $ do
    head_ $ do
      title_ "haskworks"
      style_ [type_ "text/css"] $ T.pack highlightSyntax
      fonts
      css
      jqueryJS
      materializeJS
      meta
    body_ $ do
      nav_ $ div_ [classes_ ["nav-wrapper", T.pack cardColor]] $ a_ [href_ "index.html", classes_ ["brand-logo", "center"]] "Hask works"
      section_ [class_ "section"] $ div_ [class_ "container"] content

blogPostHtmlWrapper :: BlogPostInfo -> String
blogPostHtmlWrapper info = (LT.unpack . renderText . mainContent) $
  div_ [class_ "row"] $
  div_ [classes_ ["col", "s12"]] $
  div_ [classes_ ["card", "purple darken-3", "darken-1", "white-text"]] $
  div_ [class_ "card-content"] $ do
  span_ [classes_ ["card-title"]] $ toHtml (title info)
  div_ [] $ toHtml "$blogPost$"

blogPostHolder :: Html () -> Html ()
blogPostHolder blogPostHtml = mempty

buildIndexFile :: [BlogPostInfo] -> String
buildIndexFile = LT.unpack . renderText . mainContent . foldl' mappend mempty . fmap buildPostInfo

buildPostInfo :: BlogPostInfo -> Html ()
buildPostInfo (BlogPostInfo postUrl heading date summary poster tags) =
  div_ [class_ "row"] $
  div_ [classes_ ["col", "m3", "s12"]] $
  div_ [classes_ ["card", "z-depth-3", T.pack cardColor, "white-text"]] $ do
    case poster of
      Nothing -> mempty
      Just imgUrl ->
        div_ [classes_ ["card-image", "responsive-img"]] $ img_ [src_ (T.pack imgUrl)]
    div_ [classes_ ["card-content"]] $ do
      renderTitle heading
      div_ [style_ "margin-bottom: 8px;"] $ renderTags tags
      p_ $ toHtml summary
    div_ [class_ "card-action"] $ a_ [href_ (T.pack postUrl)] "Full story"
  where
    renderTag :: String -> Html ()
    renderTag str = span_ [classes_ ["tag", "blue-text"]] $ toHtml $ T.pack str
    renderTags :: [String] -> Html ()
    renderTags tags = div_ $ (foldl' mappend mempty . fmap renderTag) tags
    renderTitle :: String -> Html ()
    renderTitle heading = div_ $ h5_ [style_ "margin: 4px;"] $ toHtml heading
