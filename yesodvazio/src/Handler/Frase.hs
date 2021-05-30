{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE QuasiQuotes #-}
module Handler.Frase where

import Import

formFrase :: Maybe Frase -> Form Frase
formFrase mf = renderDivs $ Frase
    <$> areq textField "Frase: " (fmap fraseFrase mf)
    <*> areq intField "ID do autor: " (fmap fraseIdAutor mf)
    <*> areq intField "ID da categoria: " (fmap fraseIdCategoria mf)

getFraseR :: Handler Html
getFraseR = do
    (widget,_) <- generateFormPost (formFrase Nothing)
    msg <- getMessage 
    defaultLayout (formWidgetFrase widget msg FraseR "Cadastrar")

postFraseR :: Handler Html
postFraseR = do
    ((result,_),_) <- runFormPost (formFrase Nothing)
    case result of
        FormSuccess frase -> do
            runDB $ insert frase
            setMessage [shamlet|
                <div>
                    Frase enviada com sucesso!
            |]
            redirect FraseR
        _ -> redirect HomeR


getListaFraseR :: Handler Html
getListaFraseR = do
    frases <- runDB $ selectList [] []
    defaultLayout $(whamletFile "templates/listaFrase.hamlet")

postApagarFraseR :: FraseId -> Handler Html 
postApagarFraseR fid = do
    runDB $ delete fid
    redirect ListaFraseR


getEditarFraseR :: FraseId -> Handler Html
getEditarFraseR fid = do
    frase <- runDB $ get404 fid
    (widget,_) <- generateFormPost (formFrase (Just frase))
    msg <- getMessage 
    defaultLayout (formWidgetFrase widget msg (EditarFraseR fid) "Editar")


postEditarFraseR :: FraseId -> Handler Html
postEditarFraseR fid = do
    _ <- runDB $ get404 fid
    ((result,_),_) <- runFormPost (formFrase Nothing)
    case result of
        FormSuccess novaFrase -> do
            runDB $ replace fid novaFrase
            redirect ListaFraseR
        _ -> redirect HomeR


formWidgetFrase :: Widget -> Maybe Html -> Route App -> Text -> Widget
formWidgetFrase widget msg rota m = $(whamletFile "templates/formFrase.hamlet")