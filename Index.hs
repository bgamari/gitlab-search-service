{-# LANGUAGE OverloadedStrings #-}

module Index (indexPage) where

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

          div_ [class_ "field"] $ div_ [class_ "control"] $ do
            input_ [type_ "submit", value_ "Go"]


        section_ usageSection

usageSection :: Html ()
usageSection = do
  h2_ "Usage"
  p_ "Add a keyword search to the search box below. The follow query syntax is recognized:"
  table_ $ do
    thead_ $ tr_ $ do
      th_ "Syntax"
      th_ "Meaning"
    tbody_ $ tr_ $ do
      let example :: Html () -> Html () -> Html ()
          example a b = tr_ $ do
            td_ $ code_ a
            td_ b

      example "GROUP/PROJECT#N" "Issue #N of the given project"
      example "GROUP/PROJECT issue N" "Issue #N of the given project"
      example "GROUP/PROJECT!N" "Merge request #N of the given project"
      example "GROUP/PROJECTx mr N" "Merge request #N of the given project"
      example "GROUP/PROJECT# TERMS" "Search for the given terms in issues of the given project"
      example "GROUP/PROJECT! TERMS" "Search for the given terms in merge requests of the given project"
      example "GROUP/PROJECT TERMS" "Search for the given terms the given project"
