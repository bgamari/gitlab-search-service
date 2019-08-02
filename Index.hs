{-# LANGUAGE OverloadedStrings #-}

module Index where

import Lucid

indexPage :: Html ()
indexPage =
  doctypehtml_ $ do
    head_ $ do
      title_ "GitLab Search"
      meta_ [charset_ "utf-8"]
      meta_ [name_ "viewport", content_ "width=device-width, initial-scale=1"]
      link_
        [ rel_ "stylesheet"
        , href_ "https://cdnjs.cloudflare.com/ajax/libs/bulma/0.7.5/css/bulma.min.css"
        ]

    body_ $ do
      section_ [class_ "section content"] $ do
        h1_ "GitLab Search"
        form_ [class_ "form", method_ "GET", action_ "/search"] $ do
          div_ [class_ "field"] $ div_ [class_ "control"] $ do
            label_ $ do
              "Query:"
              input_ [name_ "q"]
          div_ [class_ "field"] $ do
            div_ [class_ "control"] $ do
              input_ [type_ "submit", value_ "Go"]

