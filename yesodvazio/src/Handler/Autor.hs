{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE QuasiQuotes #-}
module Handler.Autor where

import Import
import Handler.Auxiliar

formAutor :: Maybe Autor -> Form Autor
formAutor ma = renderDivs $ Autor
    <$> areq textField "Autor:" (fmap autorAutor ma)
    

getAutorR :: Handler Html
getAutorR = do
    (widget,_) <- generateFormPost (formAutor Nothing)
    msg <- getMessage 
    defaultLayout (formWidget widget msg AutorR "Cadastrar")

postAutorR :: Handler Html
postAutorR = do
    ((result,_),_) <- runFormPost (formAutor Nothing)
    case result of
        FormSuccess autor -> do
            runDB $ insert autor
            setMessage [shamlet|
                <div>
                    Autor enviado com sucesso!
            |]
            redirect AutorR
        _ -> redirect HomeR


getListaAutorR :: Handler Html
getListaAutorR = do
    autores <- runDB $ selectList [] []
    defaultLayout $(whamletFile "templates/listaAutor.hamlet")


postApagarAutorR :: AutorId -> Handler Html 
postApagarAutorR aid = do
    runDB $ delete aid
    redirect ListaAutorR


getEditarAutorR :: AutorId -> Handler Html
getEditarAutorR aid = do
    autor <- runDB $ get404 aid
    (widget,_) <- generateFormPost (formAutor (Just autor))
    msg <- getMessage 
    defaultLayout (formWidget widget msg (EditarAutorR aid) "Editar")
        

postEditarAutorR :: AutorId -> Handler Html
postEditarAutorR aid = do
    _ <- runDB $ get404 aid
    ((result,_),_) <- runFormPost (formAutor Nothing)
    case result of
        FormSuccess novoAutor -> do
            runDB $ replace aid novoAutor
            redirect ListaAutorR
        _ -> redirect HomeR