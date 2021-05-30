{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE QuasiQuotes #-}
module Handler.Adm where

import Import
import Handler.Auxiliar

formLogin :: Form (Adm, Text)
formLogin = renderDivs $ (,) 
    <$> (Adm
        <$> areq textField "E-mail: " Nothing
        <*> areq passwordField "Senha: " Nothing
        )
    <*> areq passwordField "Confirmação: " Nothing

getAdmR :: Handler Html
getAdmR = do
    (widget,_) <- generateFormPost formLogin
    msg <- getMessage 
    defaultLayout (formWidget widget msg AdmR "Cadastrar")

postAdmR :: Handler Html
postAdmR = do
    ((result,_),_) <- runFormPost formLogin
    case result of
        FormSuccess (adm@(Adm email senha), conf) -> do
            admExiste <- runDB $ getBy (UniqueEmail email)
            case admExiste of
                Just _ -> do
                    setMessage [shamlet|
                        <div>
                            E-MAIL JÁ CADASTRADO!
                    |]
                    redirect AdmR
                Nothing -> do
                    if senha == conf then do
                        runDB $ insert adm
                        setMessage [shamlet|
                            <div>
                                Administrador inserido com sucesso!
                        |]
                        redirect AdmR
                    else do
                        setMessage [shamlet|
                            <div>
                                Senha e confirmação diferentes!
                        |]
                        redirect AdmR            
        _ -> redirect HomeR