{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE QuasiQuotes #-}
module Handler.Calculos where

import Import

-- # eh o interpolador que printa strings na pagina
-- http://localhost:8080/soma/n1/15/n2/8
getSomaR :: Int -> Int -> Handler Html
getSomaR n1 n2 = do
    res <- return (show (n1+n2))
    defaultLayout $ do
        [whamlet|
            <h1>
                A SOMA EH: #{res}
        |]