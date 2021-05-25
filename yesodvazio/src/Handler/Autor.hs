{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE QuasiQuotes #-}
module Handler.Autor where

import Import

formAutor :: Form Autor
formAutor = renderDivs $ Autor
    <$> areq textField "Autor:" Nothing
    

getAutorR :: Handler Html
getAutorR = do
    (widget,_) <- generateFormPost formAutor
    msg <- getMessage 
    defaultLayout $ do
        [whamlet|
            $maybe mensa <- msg
                <div>
                    ^{mensa}
            <form method=post action=@{AutorR}>
                ^{widget}
                <input type="submit" value="Cadastrar">
        |]

postAutorR :: Handler Html
postAutorR = do
    ((result,_),_) <- runFormPost formAutor
    case result of
        FormSuccess autor -> do
            runDB $ insert autor
            setMessage [shamlet|
                <div>
                    Autor enviado com sucesso!
            |]
            redirect AutorR
        _ -> redirect HomeR


-- /adm/cadastro/autor AutorR GET POST
-- /adm/#AutorId/apagar ApagarAutorR POST

getListaAutorR :: Handler Html
getListaAutorR = do
    autores <- runDB $ selectList [] []
    defaultLayout $(whamletFile "templates/listaAutor.hamlet")

postApagarAutorR :: AutorId -> Handler Html 
postApagarAutorR aid = do
    runDB $ delete aid
    redirect AutoresR