{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE QuasiQuotes #-}
module Handler.Cliente where

import Import

formCliente :: Form Cliente
formCliente = renderDivs $ Cliente
    <$> areq textField "Informe seu nome: " Nothing
    <*> areq textField "Informe seu e-mail: " Nothing
    <*> areq textField "Digite a frase: " Nothing
    <*> areq textField "Informe o autor da frase: " Nothing
    <*> areq textField "Informe a categoria da frase: " Nothing

getClienteR :: Handler Html
getClienteR = do
    (widget,_) <- generateFormPost formCliente
    msg <- getMessage 
    defaultLayout $ do
        [whamlet|
            $maybe mensa <- msg
                <div>
                    ^{mensa}
            <form method=post action=@{ClienteR}>
                ^{widget}
                <input type="submit" value="Cadastrar">
        |]

postClienteR :: Handler Html
postClienteR = do
    ((result,_),_) <- runFormPost formCliente
    case result of
        FormSuccess cliente -> do
            runDB $ insert cliente
            setMessage [shamlet|
                <div>
                    Frase enviada com sucesso!
            |]
            redirect ClienteR
        _ -> redirect HomeR