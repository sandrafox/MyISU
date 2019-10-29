module PodDB where

    import Database.HDBC
    import Database.HDBC.Sqlite3
    import PodTypes
    import Control.Monad(when, forM)
    import Data.List(sort)
    import qualified Data.Text.Lazy as TL
    import Web.Scotty.Internal.Types (ActionT)
    import Control.Monad.IO.Class
    import Data.Pool
    
    -- | Initialize DB and return database Connection
    connect :: FilePath -> IO Connection
    connect fp =
        do dbh <- connectSqlite3 fp
           prepDB dbh
           return dbh
    
    {- | Prepare the database for our data.
    
    We create two tables and ask the database engine to verify some pieces
    of data consistency for us:
    
    * castid and epid both are unique primary keys and must never be duplicated
    * castURL also is unique
    * In the episodes table, for a given podcast (epcast), there must be only
      one instance of each given URL or episode ID
    -}
    prepDB :: IConnection conn => conn -> IO ()
    prepDB dbh =
        do tables <- getTables dbh
           when (not ("posts" `elem` tables)) $
               do run dbh "CREATE TABLE posts (\
                           \postId INTEGER NOT NULL PRIMARY KEY AUTOINCREMENT,\
                           \authorId INTEGER NOT NULL,\
                           \title TEXT NOT NULL,\
                           \text TEXT NOT NULL,\
                           \students TEXT,\
                           \UNIQUE(authorId, title),\
                           \UNIQUE(title, postId))" []
                  return ()
           when (not ("users" `elem` tables)) $
               do run dbh "CREATE TABLE users (\
                           \userId INTEGER NOT NULL PRIMARY KEY AUTOINCREMENT,\
                           \login TEXT NOT NULL UNIQUE,\
                           \pass TEXT NOT NULL,\
                           \name TEXT NOT NULL,\
                           \student BOOL NOT NULL,\
                           \lecturer BOOL NOT NULL,\
                           \admin BOOL NOT NULL)" []
                  run dbh "INSERT INTO users (login, pass, name, student, lecturer, admin) VALUES (?, ?, ?, ?, ?, ?)" 
                          [toSql "admin", toSql "admin", toSql "admin", toSql False, toSql False, toSql True]
                  run dbh "INSERT INTO users (login, pass, name, student, lecturer, admin) VALUES (?, ?, ?, ?, ?, ?)" 
                          [toSql "general", toSql "general", toSql "general", toSql True, toSql True, toSql True]
                  return ()
           when (not ("docs" `elem` tables)) $
               do run dbh "CREATE TABLE docs (\
                           \docId INTEGER NOT NULL PRIMARY KEY AUTOINCREMENT,\
                           \docURL TEXT NOT NULL,\
                           \postId INTEGER NOT NULL,\
                           \docType TEXT NOT NULL,\
                           \UNIQUE(postId, docURL),\
                           \UNIQUE(postId, docId))" []
                  return ()
           when (not ("students" `elem` tables)) $
               do run dbh "CREATE TABLE students (\
                            \studentId INTEGER NOT NULL PRIMARY KEY,\
                            \groupName TEXT NOT NULL,\
                            \course INTEGER NOT NULL,\
                            \english TEXT NOT NULL)" []
                  run dbh "INSERT INTO students (studentId, groupName, course, english) VALUES (2, ?, ?, ?)"
                          [toSql "M3435", toSql (4 :: Int), toSql "4ИТИПКТB2"]
                  return ()     
           commit dbh
    
    {- | Adds a new podcast to the database.  Ignores the castid on the
    incoming podcast, and returns a new object with the castid populated.
    
    An attempt to add a podcast that already exists is an error. -}
    addUser :: Pool Connection -> User -> String -> ActionT TL.Text IO ()
    addUser dbh user password = do 
            liftIO $ runSqlWithTransaction dbh "INSERT INTO users (login, pass, name, student, lecturer, admin) VALUES (?, ?, ?, ?, ?, ?)"
                 [toSql (login user), toSql password, toSql (name user), toSql (student user), toSql (lecturer user), toSql (admin user)]
            return ()
                 {-where errorHandler e = 
                  do fail $ "Error adding author; is this name already used?\n"
                         ++ show e-}

    addStudent :: Pool Connection -> Student -> ActionT TL.Text IO ()
    addStudent pool student = do
        liftIO $ runSqlWithTransaction pool "INSERT INTO students (studentId, groupName, course, english) VALUES (?, ?, ?, ?)"
                 [toSql (studentId student), toSql (group student), toSql (course student), toSql (english student)]
        return ()
    
    {- | Adds a new episode to the database. 
    
    Since this is done by automation, instead of by user request, we will
    simply ignore requests to add duplicate episodes.  This way, when we are
    processing a feed, each URL encountered can be fed to this function,
    without having to first look it up in the DB.
    
    Also, we generally won't care about the new ID here, so don't bother
    fetching it. -}
    addPost :: Pool Connection -> Post -> ActionT TL.Text IO ()
    addPost dbh post =
        liftIO $ runSqlWithTransaction dbh "INSERT OR IGNORE INTO posts (authorId, title, text, limitations) \
                    \VALUES (?, ?, ?)"
                    [toSql (userId . author $ post), toSql $ title post, toSql $ text post, toSql . show $ limitations post]
        >> return ()

    addDoc :: Pool Connection -> Doc -> IO ()
    addDoc dbh doc = 
        liftIO $ runSqlWithTransaction dbh "INSERT OR IGNORE INTO docs (docURL, postId, docType) \
                 \VALUES (?, ?)"
                 [toSql (docURL doc), toSql (postId . post $ doc), toSql $ show . docType $ doc]
        >> return ()
           
    {- | Modifies an existing podcast.  Looks up the given podcast by
    ID and modifies the database record to match the passed Podcast. -}
    updateStudent :: Pool Connection -> Student -> ActionT TL.Text IO ()
    updateStudent dbh student = do
        liftIO $ runSqlWithTransaction dbh "UPDATE students SET groupName = ? course = ? english = ? WHERE studentId = ?" 
                [toSql $ group student, toSql $ course student, toSql $ english student, toSql $ studentId student]
        return ()
    
    runSqlWithTransaction :: Pool Connection -> String -> [SqlValue] -> IO Integer
    runSqlWithTransaction pool query values = withResource pool ins
                                         where ins conn = withTransaction conn (\c -> run c query values)

    fetchSql :: Pool Connection -> String -> [SqlValue] -> IO [[SqlValue]]
    fetchSql pool query values = withResource pool ins
                                         where ins conn = quickQuery' conn query values 

    {- | Remove a podcast.  First removes any episodes that may exist
    for this podcast. -}
    removePost :: Pool Connection -> Int -> ActionT TL.Text IO ()
    removePost dbh id =
        do liftIO $ runSqlWithTransaction dbh "DELETE FROM posts WHERE postId = ?" [toSql id]
           liftIO $ runSqlWithTransaction dbh "DELETE FROM docs WHERE postId = ?" [toSql id]
           return ()

    removeUser :: Pool Connection -> Int -> ActionT TL.Text IO ()
    removeUser dbh id =
        do --posts <- fetchSql dbh "SELECT postId FROM posts WHERE authorId = ?" [toSql id]
           --delDocs <- prepare dbh "DELETE FROM docs WHERE postId = ?"
           --executeMany delDocs (fmap (fmap toSql) posts)
           liftIO $ runSqlWithTransaction dbh "DELETE FROM posts WHERE authorId = ?" [toSql id]
           liftIO $ runSqlWithTransaction dbh "DELETE FROM students WHERE studentId = ?" [toSql id]
           liftIO $ runSqlWithTransaction dbh "DELETE FROM authors WHERE userId = ?" [toSql id]
           return ()

    removeDoc :: Pool Connection -> Int -> ActionT TL.Text IO ()
    removeDoc dbh id =
        do liftIO $ runSqlWithTransaction dbh "DELETE FROM docs WHERE docId = ?" [toSql id]
           return ()

    convPostWithAuthorRow :: Pool Connection -> [SqlValue] -> IO Post
    convPostWithAuthorRow dbh [pId, aId, n, t, l] = 
        do author <- liftIO $ getUser dbh (fromSql aId)
           case author of
             Just a -> return (Post {postId = fromSql pId, author = a, 
             title = fromSql n, text = fromSql t, limitations = read $ fromSql l})
             Nothing -> fail $ "Really bad error; can't find author of podcast"
    
    getPostIdByAuthorAndTitle :: Pool Connection -> Int -> String -> IO (Maybe Int)
    getPostIdByAuthorAndTitle pool aId t = do
        id <- liftIO $ fetchSql pool "SELECT postId, FROM posts WHERE authorId = ? AND title = ?" [toSql aId, toSql t]
        case id of
            [[x]] -> return $ Just $ fromSql x
            _ -> return Nothing
    
             {- | Gets a list of all podcasts. -}
    getPosts :: Pool Connection -> IO [Post]
    getPosts dbh =
        do res <- liftIO $ fetchSql dbh 
                  "SELECT * FROM posts ORDER BY postId" []
           forM res (convPostWithAuthorRow dbh)
    
    {- | Get a particular podcast.  Nothing if the ID doesn't match, or
    Just Podcast if it does. -}
    getPostwithoutAuthor :: Pool Connection -> Int -> IO (Maybe (Post, Int))
    getPostwithoutAuthor dbh wantedId =
        do res <- liftIO $ fetchSql dbh 
                  "SELECT postId, authorid, text FROM posts WHERE postId = ?" [toSql wantedId]
           case res of
             [x] -> return (Just (convPostRow x))
             [] -> return Nothing
             x -> fail $ "Really bad error; more than one podcast with ID"

    nullAuthor :: User
    nullAuthor = User {userId = -1,
                         login = "",
                         name = "",
                         student = False,
                         lecturer = False,
                         admin = False}
    
    
                         {- | Convert the result of a SELECT into a Podcast record -}
    convPostRow ::[SqlValue] -> (Post, Int)
    convPostRow [pId, aId, n, t, l] =
        (Post {postId = fromSql pId, author = nullAuthor, 
        title = fromSql n, text = fromSql t, limitations = read $ fromSql l}, fromSql aId)
    convPostRow x = error $ "Can't convert post row " ++ show x

    getPost :: Pool Connection -> Int -> IO (Maybe Post)
    getPost dbh wantedId =
        do res <- getPostwithoutAuthor dbh wantedId
           case res of
              Nothing -> return Nothing
              Just (post, aId) -> newPost dbh post aId


    newPost :: Pool Connection -> Post -> Int -> IO (Maybe Post)
    newPost dbh post wantedId =
        do author <- getUser dbh wantedId
           case author of
              Just a -> return (Just (Post {postId = postId post, 
                                            author = a, 
                                            title = title post, 
                                            text = text post, 
                                            limitations = limitations post}))
              Nothing -> return Nothing

    getUser :: Pool Connection -> Int -> IO (Maybe User)
    getUser dbh wantedId = 
        do res <- liftIO $ fetchSql dbh 
                  "SELECT authorId, login, name, student, lecturer, admin FROM users WHERE userId = ?" [toSql wantedId]
           case res of
              [x] -> return (Just (convUserRow x))
              [] -> return Nothing
              x -> fail $ "Really bad error; more than one author with ID"

    getUserByLoginAndPassword :: Pool Connection -> Maybe (String, String) -> IO (Maybe User)
    getUserByLoginAndPassword _ Nothing = return Nothing
    getUserByLoginAndPassword pool (Just (login, pass)) = do
        user <- liftIO $ fetchSql pool "Select * FROM users where login = ?" [toSql login]
        return $ res user
        where res [[id, ln, pwd, n, s, l, a]] = if ((fromSql pwd) == pass) then
                                                      Just (User {userId = fromSql id,
                                                                  login = fromSql ln,
                                                                  name = fromSql n,
                                                                  student = fromSql s,
                                                                  lecturer = fromSql l,
                                                                  admin = fromSql a})
                                                else Nothing
              res _ = Nothing

    getUserByLogin :: Pool Connection -> String -> IO (Maybe User)
    getUserByLogin pool login = do
        user <- liftIO $ fetchSql pool "Select * FROM users where login = ?" [toSql login]
        return $ res user
        where res [[id, ln, pwd, n, s, l, a]] = Just (User {userId = fromSql id,
                                                            login = fromSql ln,
                                                            name = fromSql n,
                                                            student = fromSql s,
                                                            lecturer = fromSql l,
                                                            admin = fromSql a})
              res _ = Nothing

    getStudentById :: Pool Connection -> Int -> IO (Maybe Student)
    getStudentById pool id = do
        student <- liftIO $ fetchSql pool "Select * FROM students where studentId = ?" [toSql id]
        return $ res student
        where res [[i, g, c, e]] = Just (Student {studentId = fromSql i,
                                                  group = fromSql g,
                                                  course = fromSql c,
                                                  english = fromSql e})
              res _ = Nothing

    convUserRow :: [SqlValue] -> User
    convUserRow [aId, l, n, s, lr, a] = User {userId = fromSql aId,
                                              login = fromSql l,
                                              name = fromSql n,
                                              student = fromSql s,
                                              lecturer = fromSql lr,
                                              admin = fromSql a}
    convAuthorRow x = error $ "Can't convert author row " ++ show x

    getPostDocs :: Pool Connection -> Post -> IO [Doc]
    getPostDocs dbh post =
        do r <- liftIO $ fetchSql dbh "SELECT docId, docURL, docType FROM docs WHERE postId = ?" [toSql (postId post)]
           return $ map convDocRow r
        where convDocRow [pId, pURL, pType] = Doc {docId = fromSql pId, 
            docURL = fromSql pURL, post = post, docType = read $ fromSql pType}