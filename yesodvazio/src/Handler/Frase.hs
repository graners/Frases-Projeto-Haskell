{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE QuasiQuotes #-}
module Handler.Frase where

import Import

formFrase :: Form Frase
formFrase = renderDivs $ Frase
    <$> areq textField "Frase:" Nothing
    <*> areq intField "ID do autor:" Nothing
    <*> areq intField "ID da categoria:" Nothing

getFraseR :: Handler Html
getFraseR = do
    (widget,_) <- generateFormPost formFrase
    msg <- getMessage 
    defaultLayout $ do
        [whamlet|
            $maybe mensa <- msg
                <div>
                    ^{mensa}
            <form method=post action=@{FraseR}>
                ^{widget}
                <input type="submit" value="Cadastrar">
        |]

postFraseR :: Handler Html
postFraseR = do
    ((result,_),_) <- runFormPost formFrase
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
    redirect FraseR