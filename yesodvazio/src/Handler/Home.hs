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
import Database.Persist.Postgresql
--import Network.HTTP.Types.Status
--import Database.Persist.Postgresql

getCategoriasR :: Handler Html
getCategoriasR = do
    categorias <- runDB $ selectList [] []
    defaultLayout $ do
        toWidgetHead $(luciusFile "templates/categorias.lucius")
        $(whamletFile "templates/categorias.hamlet")

getAutoresR :: Handler Html
getAutoresR = do
    autores <- runDB $ selectList [] []
    defaultLayout $ do
        toWidgetHead $(luciusFile "templates/autores.lucius")
        $(whamletFile "templates/autores.hamlet")

getFrasesR :: Handler Html
getFrasesR = do
    let sql = "SELECT ??,??,?? FROM frase \
        \ INNER JOIN autor ON autor.id = frase.autid \
        \ INNER JOIN categoria ON categoria.id = frase.catid "
    tudo <- runDB $ rawSql sql [] :: Handler [(Entity Frase, Entity Autor, Entity Categoria)]
    defaultLayout $ do
        toWidgetHead $(luciusFile "templates/frases.lucius")    
        $(whamletFile "templates/frases.hamlet")

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