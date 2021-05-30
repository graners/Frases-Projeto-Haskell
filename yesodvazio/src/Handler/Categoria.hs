{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE QuasiQuotes #-}
module Handler.Categoria where

import Import
import Handler.Auxiliar

formCategoria :: Maybe Categoria -> Form Categoria
formCategoria mc = renderDivs $ Categoria
    <$> areq textField "Categoria:" (fmap categoriaCategoria mc)


getCategoriaR :: Handler Html
getCategoriaR = do
    (widget,_) <- generateFormPost (formCategoria Nothing)
    msg <- getMessage 
    defaultLayout (formWidget widget msg CategoriaR "Cadastrar")


postCategoriaR :: Handler Html
postCategoriaR = do
    ((result,_),_) <- runFormPost (formCategoria Nothing)
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
    redirect ListaCategoriaR


getEditarCategoriaR :: CategoriaId -> Handler Html
getEditarCategoriaR cid = do
    categoria <- runDB $ get404 cid
    (widget,_) <- generateFormPost (formCategoria (Just categoria))
    msg <- getMessage 
    defaultLayout (formWidget widget msg (EditarCategoriaR cid) "Editar")


postEditarCategoriaR :: CategoriaId -> Handler Html
postEditarCategoriaR cid = do
    _ <- runDB $ get404 cid
    ((result,_),_) <- runFormPost (formCategoria Nothing)
    case result of
        FormSuccess novaCategoria -> do
            runDB $ replace cid novaCategoria
            redirect ListaCategoriaR
        _ -> redirect HomeR
