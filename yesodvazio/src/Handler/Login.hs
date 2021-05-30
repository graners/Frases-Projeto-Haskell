{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE QuasiQuotes #-}
module Handler.Login where

import Import
import Handler.Auxiliar

formLogin :: Form Adm
formLogin = renderDivs $ Adm
    <$> areq textField "E-mail: " Nothing
    <*> areq passwordField "Senha: " Nothing

getAutR :: Handler Html
getAutR = do
    (widget,_) <- generateFormPost formLogin
    msg <- getMessage 
    defaultLayout (formWidget widget msg AutR "Entrar")

postAutR :: Handler Html
postAutR = do
    ((result,_),_) <- runFormPost formLogin
    case result of
        FormSuccess (Adm email senha) -> do
            admExiste <- runDB $ getBy (UniqueEmail email)
            case admExiste of
                Nothing -> do
                    setMessage [shamlet|
                        ADM NÃO CADASTRADO!
                    |]
                    redirect AutR
                Just (Entity _ adm) -> do
                    if senha == admSenha adm then do
                        -- Fazendo isso, estou logado
                        setSession "_ID" (admEmail adm)
                        redirect HomeR
                    else do
                        setMessage [shamlet|
                            E-MAIL E/OU SENHA NÃO CONFEREM!
                        |]
                    redirect AutR
        _ -> redirect HomeR


postSairR :: Handler Html
postSairR = do
    deleteSession "_ID"
    redirect HomeR