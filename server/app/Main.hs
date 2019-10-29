{-# LANGUAGE OverloadedStrings #-}

module Main where

import Web.Scotty
import Database.HDBC
import Data.Pool
import Control.Monad.IO.Class
import Control.Monad
import qualified Data.Text.Lazy as TL
import qualified Data.Map.Strict as M
import qualified Data.List as L
import PodDB
import qualified PodTypes as T

main :: IO ()
main = do
    pool <- createPool (connect "poddb.db") disconnect 1 5 10
    scotty 3000 $ do
        get "/" $ json ("OK" :: String)
        --login user
        get "/login" $ do
            body <- jsonData :: ActionM (String, String)
            user <- liftIO $ getUserByLoginAndPassword pool $ Just body
            viewUser user 
        get "/student/:id" $ do
            id <- param "id" :: ActionM TL.Text
            student <- liftIO $ getStudentById pool (read (TL.unpack id))
            case student of
                Nothing -> json ()
                (Just s) -> json s
        --get all posts for user
        get "/student/:id/posts" $ do
            id <- param "id" :: ActionM TL.Text 
            student <- liftIO $ getStudentById pool (read (TL.unpack id)) 
            allPosts <- liftIO $ getPosts pool
            viewPosts $ neededPosts student allPosts (\post -> checkLimitations student $ T.limitations post)
        --get all posts which contains string
        get "/student/:id/posts/find" $ do
            id <- param "id" :: ActionM TL.Text
            student <- liftIO $ getStudentById pool (read (TL.unpack id))
            body <- jsonData :: ActionM String
            allPosts <- liftIO $ getPosts pool
            viewPosts $ neededPosts student allPosts 
                       (\post -> (checkLimitations student $ T.limitations post) 
                                 && (contains post body))
        --get user by login
        get "/admin/:login" $ do
            login <- param "login" :: ActionM String
            user <- liftIO $ getUserByLogin pool login
            viewUser user
        --get doc from post
        get "/post/:id/docs" $ do
            id <- param "id" :: ActionM Int
            post <- liftIO $ getPost pool id
            case post of
                Nothing -> json()
                Just p -> do ds <- liftIO $ getPostDocs pool p
                             json ds
        --create post
        post "/lecturer/post" $ do
            body <- jsonData :: ActionM (T.Post, [T.Doc])
            addPost pool $ fst body
            mid <- liftIO $ getPostIdByAuthorAndTitle pool (T.userId . T.author $ fst body) (T.title $ fst body)
            case mid of
                Nothing -> json()
                Just id -> do liftIO $ forM (map (addPostId id) $ snd body) $ addDoc pool
                              json id  
        --create user
        post "admin/user" $ do
            body <- jsonData :: ActionM (T.User, String)
            addUser pool (fst body) (snd body)
            json() 
        --add student
        post "admin/user/student" $ do
            body <- jsonData :: ActionM T.Student
            addStudent pool body
            json()
        --modify student
        put "admin/user/student" $ do
            body <- jsonData :: ActionM T.Student
            updateStudent pool body
            json()
        --delete post
        delete "lecturer/post/:id" $ do
            id <- param "id" :: ActionM Int 
            removePost pool id
            json()
        --delete user
        delete "admin/user/:id" $ do
            id <- param "id" :: ActionM Int 
            removeUser pool id
            json()

getLoginAndPass b = M.lookup "login"  b >>=
    \login  -> M.lookup "password" b >>=
    \pass -> Just (login, pass)

viewUser :: Maybe T.User -> ActionM ()
viewUser Nothing = json ()
viewUser (Just user) = json (user)

viewPosts :: [T.Post] -> ActionM ()
viewPosts posts = json posts

neededPosts :: Maybe T.Student -> [T.Post] -> (T.Post -> Bool) -> [T.Post]
neededPosts (Just student) posts f = filter f posts
                            where   
neededPosts Nothing _ _ = []

checkLimitations :: Maybe T.Student -> T.Limitation -> Bool
checkLimitations Nothing _ = False
checkLimitations student (T.Or l1 l2) = (checkLimitations student l1) || (checkLimitations student l2)
checkLimitations (Just student) (T.Group g) = (T.group student) == g
checkLimitations (Just student) (T.Course c) = (T.course student) == c
checkLimitations (Just student) (T.English e) = (T.english student) == e

contains :: T.Post -> String -> Bool
contains _ "" = True
contains (T.Post _ author title text _) s = (L.isInfixOf s $ T.name author) || (L.isInfixOf s title) || (L.isInfixOf s text)

addPostId :: Int -> T.Doc -> T.Doc
addPostId id (T.Doc did durl (T.Post _ a t tt l) dt) = 
    T.Doc {T.docId = did,
           T.docURL = durl,
           T.post = T.Post {T.postId = id,
                            T.author = a,
                            T.title = t,
                            T.text = tt,
                            T.limitations = l},
           T.docType = dt} 