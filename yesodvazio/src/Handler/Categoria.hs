{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE QuasiQuotes #-}
module Handler.Categoria where

import Import

formCategoria :: Form Categoria
formCategoria = renderDivs $ Categoria
    <$> areq textField "Categoria:" Nothing


getCategoriaR :: Handler Html
getCategoriaR = do
    (widget,_) <- generateFormPost formCategoria
    msg <- getMessage 
    defaultLayout $ do
        [whamlet|
            $maybe mensa <- msg
                <div>
                    ^{mensa}
            <form method=post action=@{CategoriaR}>
                ^{widget}
                <input type="submit" value="Cadastrar">
        |]


postCategoriaR :: Handler Html
postCategoriaR = do
    ((result,_),_) <- runFormPost formCategoria
    case result of
        FormSuccess categoria -> do
            runDB $ insert categoria
            setMessage [shamlet|
                <div>
                    Categoria enviada com sucesso!
            |]
            redirect CategoriaR
        _ -> redirect HomeR


getListaCategoriaR :: Handler Html
getListaCategoriaR = do
    categorias <- runDB $ selectList [] []
    defaultLayout $(whamletFile "templates/listaCategoria.hamlet")

postApagarCategoriaR :: CategoriaId -> Handler Html 
postApagarCategoriaR cid = do
    runDB $ delete cid
    redirect CategoriaR