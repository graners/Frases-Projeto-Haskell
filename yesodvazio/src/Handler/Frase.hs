{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE QuasiQuotes #-}
module Handler.Frase where

import Import
import Handler.Auxiliar
import Database.Persist.Postgresql

formFrase :: Maybe Frase -> Form Frase
formFrase mf = renderDivs $ Frase
    <$> areq textField "Frase: " (fmap fraseFrase mf)
    <*> areq (selectField autCB) "Autor: " Nothing
    <*> areq (selectField catCB) "Autor: " Nothing

autCB = do
    autores <- runDB $ selectList [] [Asc AutorAutor]
    optionsPairs $
        map (\r -> (autorAutor $ entityVal r, entityKey r)) autores

catCB = do
    categorias <- runDB $ selectList [] [Asc CategoriaCategoria]
    optionsPairs $
        map (\r -> (categoriaCategoria $ entityVal r, entityKey r)) categorias

getFraseR :: Handler Html
getFraseR = do
    (widget,_) <- generateFormPost (formFrase Nothing)
    msg <- getMessage 
    defaultLayout (formWidget widget msg FraseR "Cadastrar")

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
    -- let sql = "SELECT ??,??,?? FROM frase \
    --     \ INNER JOIN autor ON autor.id = frase.autid \
    --     \ INNER JOIN categoria ON categoria.id = frase.catid \
    --     \ WHERE frase.id = ?"
    -- frase <- runDB $ get404 fid
    -- tudo <- rumDB $ rawSql sql [toPersistValue fid] :: Handler [(Entity Frase, Entity Autor, Entity Categoria)]
    -- defaultLayout $ do
    --     [whamlet|
    --        <h1>
    --            Frases
    --        <ul>
    --            $forall (Entity _ frase, Entity _ _, Entity _ _) <- tudo
    --                <li>
    --                    #{fraseFrase frase}
    --    |]
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
    defaultLayout (formWidget widget msg (EditarFraseR fid) "Editar")


postEditarFraseR :: FraseId -> Handler Html
postEditarFraseR fid = do
    _ <- runDB $ get404 fid
    ((result,_),_) <- runFormPost (formFrase Nothing)
    case result of
        FormSuccess novaFrase -> do
            runDB $ replace fid novaFrase
            redirect ListaFraseR
        _ -> redirect HomeR
