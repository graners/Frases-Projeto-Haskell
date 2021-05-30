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
    defaultLayout $ do
        $(whamletFile "templates/categorias.hamlet")

getAutoresR :: Handler Html
getAutoresR = do
    defaultLayout $ do
        $(whamletFile "templates/autores.hamlet")
        toWidgetHead $(luciusFile "templates/autores.lucius")

getFrasesR :: Handler Html
getFrasesR = do
    defaultLayout $ do
        $(whamletFile "templates/frases.hamlet")


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