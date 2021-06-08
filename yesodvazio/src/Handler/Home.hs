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

getCategoriasR :: Handler Html
getCategoriasR = do
    categorias <- runDB $ selectList [] []
    defaultLayout $(whamletFile "templates/categorias.hamlet")

getAutoresR :: Handler Html
getAutoresR = do
    autores <- runDB $ selectList [] []
    defaultLayout $(whamletFile "templates/autores.hamlet")

getFrasesR :: Handler Html
getFrasesR = do
    frases <- runDB $ selectList [] []
    defaultLayout $(whamletFile "templates/frases.hamlet")


getHomeR :: Handler Html
getHomeR = do
    defaultLayout $ do
        -- css estatico (bootstrap)
        adm <- lookupSession "_ID"
        addStylesheet (StaticR css_bootstrap_css)
        -- Javascript
        toWidgetHead $(juliusFile "templates/home.julius")
        -- css dinâmico (template css)
        toWidgetHead $(luciusFile "templates/home.lucius")
        -- html
        $(whamletFile "templates/home.hamlet")