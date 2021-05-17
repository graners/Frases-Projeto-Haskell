{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE QuasiQuotes #-}
module Handler.Home where

import Import
import Text.Lucius
import Text.Julius
--import Network.HTTP.Types.Status
--import Database.Persist.Postgresql

getPage1R :: Handler Html
getPage1R = do
    defaultLayout $ do
        [whamlet|
            <h1>
                PAGINA 1

            <img src=@{StaticR img_smile_jpg}>
        |]

getPage2R :: Handler Html
getPage2R = do
    defaultLayout $ do
        [whamlet|
            <h1>
                PAGINA 2
        |]

getPage3R :: Handler Html
getPage3R = do
    defaultLayout $ do
        [whamlet|
            <h1>
                PAGINA 3
        |]


getHomeR :: Handler Html
getHomeR = do
    defaultLayout $ do
        -- css estatico (bootstrap)
        addStylesheet (StaticR css_bootstrap_css)
        -- Javascript
        toWidgetHead $(juliusFile "templates/home.julius")
        -- css dinâmico (template css)
        toWidgetHead $(luciusFile "templates/home.lucius")
        -- html
        $(whamletFile "templates/home.hamlet")