{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE OverloadedStrings #-}

module ConnectMethods where

import Network.HTTP.Client
import Network.HTTP.Client.TLS
import Data.Aeson
import Data.ByteString
import Data.ByteString.Lazy
import Control.Exception
import ClassyPrelude
import Control.Monad.Catch
import Network.HTTP.Types.Status
import Data.Has
import Data.Text
import ConnectTypes
import PodTypes

startConnect :: HttpClient r m => m (Either ConnectToServerError ())
startConnect = do
    State initReq mgr <- asks getter
    let req = initReq {method = "GET", 
                       path = "/"}
    resp <- liftIO $ httpLbs req mgr
    case responseStatus resp of
        (Status 200 _) -> return $ Right ()
        _ -> Left <$> parseOrErr req resp

loginConnect :: HttpClient r m => (Text, Text) -> m User
loginConnect pair = do
    State initReq mgr <- asks getter
    let req = initReq {method = "GET",
                       path = "/login",
                       requestBody = RequestBodyLBS $ encode pair}
    resp <- liftIO $ httpLbs req mgr
    case responseStatus resp of
        (Status 200 _) -> parseOrErr req resp
        _ ->  throw $ UnexpectedResponse req resp

getPostsConnect :: HttpClient r m => Int -> m [Post]
getPostsConnect id = do
    State initReq mgr <- asks getter
    let req = initReq {method = "GET",
                       path = fromString ("/student/" Prelude.++ (show id) Prelude.++ "/posts")}
    resp <- liftIO $ httpLbs req mgr
    case responseStatus resp of
        (Status 200 _) -> parseOrErr req resp
        _ ->  throw $ UnexpectedResponse req resp

findPostsConnect :: HttpClient r m => Int -> Text -> m [Post]
findPostsConnect id text = do
    State initReq mgr <- asks getter
    let req = initReq {method = "GET",
                       path = fromString ("/student/" Prelude.++ (show id) Prelude.++ "/posts/find"),
                       requestBody = RequestBodyLBS $ encode text}
    resp <- liftIO $ httpLbs req mgr
    case responseStatus resp of
        (Status 200 _) -> parseOrErr req resp
        _ ->  throw $ UnexpectedResponse req resp

getStudentConnect :: HttpClient r m => Int -> m Student
getStudentConnect id = do
    State initReq mgr <- asks getter
    let req = initReq {method = "GET",
                       path = fromString ("/student/" Prelude.++ (show id))}
    resp <- liftIO $ httpLbs req mgr
    case responseStatus resp of
        (Status 200 _) -> parseOrErr req resp
        _ ->  throw $ UnexpectedResponse req resp