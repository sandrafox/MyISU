module PodTypes where

import Data.Aeson.TH

data Limitation = Or Limitation Limitation 
                  | Group String 
                  | Course Int 
                  | English String deriving (Eq, Show, Read)

{-instance Show Limitation where
    show (Or l1 l2) = show l1 ++ "," ++ show l2
    show (Group g) = g
    show (Course c) = show c
    show (English e) = e-}

data User = User {userId :: Int,
                  login :: String,
                  name :: String,
                  student :: Bool,
                  lecturer :: Bool,
                  admin :: Bool} deriving (Eq, Show, Read)

data Post = Post {postId :: Int,
                  author :: User,
                  title :: String,
                  text :: String,
                  limitations :: Limitation} deriving (Eq, Show, Read)

data DocType = PDF | DOC | MP3 deriving (Eq, Show, Read)

data Doc = Doc {docId :: Int,
                docURL :: String,
                post :: Post,
                docType :: DocType}

data Student = Student {studentId :: Int,
                        group :: String,
                        course :: Int,
                        english :: String}

$(deriveJSON defaultOptions ''Post)

$(deriveJSON defaultOptions ''Limitation)

$(deriveJSON defaultOptions ''User)

$(deriveJSON defaultOptions ''Student)

$(deriveJSON defaultOptions ''Doc)

$(deriveJSON defaultOptions ''DocType)






