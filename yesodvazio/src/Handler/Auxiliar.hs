{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE QuasiQuotes #-}
module Handler.Auxiliar where

import Import

formWidget :: Widget -> Maybe Html -> Route App -> Text -> Widget
formWidget widget msg rota m = $(whamletFile "templates/form.hamlet")