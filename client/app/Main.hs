{-# LANGUAGE OverloadedStrings #-}

{-# LANGUAGE OverloadedLabels  #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Main where

import Data.GI.Base
import Data.GI.Base.Signals
import Data.GI.Base.Properties
import GI.GLib
import GI.GObject
import qualified GI.Gtk as G
import qualified GI.Gtk.Objects.Button as G
import qualified GI.Gtk.Objects.Entry as G
import qualified GI.Gtk.Objects.Label as G
import GI.Gdk
import System.Environment.FindBin
import Data.Text
import Control.Monad.IO.Class
import Control.Monad.Reader
import ConnectTypes
import ConnectMethods
import qualified PodTypes as T

data GUI = GUI {
        loginWin :: G.Window,
        loginEntry :: G.Entry,
        passEntry :: G.Entry,
        loginBtn :: G.Button,
        mainWin :: G.Window,
        generalBtn :: G.Button,
        studentBtn :: G.Button,
        lecturerBtn :: G.Button,
        adminBtn :: G.Button,
        generalBox :: G.Box,
        nameLabel :: G.Label,
        infoLabel :: G.Label,
        studentBox :: G.Box,
        groupLabel :: G.Label,
        findEntry :: G.Entry,
        findBtn :: G.Button,
        vboxes :: [G.Box],
        titleLabels :: [G.Label],
        contentLabels :: [G.Label],
        downloadBtns :: [G.Button],
        lecturerBox :: G.Box,
        addPostBtn :: G.Button,
        adminBox :: G.Box,
        addUserBtn :: G.Button,
        delUserBtn :: G.Button,
        addStudentBtn :: G.Button,
        modStudentBtn :: G.Button
    }

data StateGUI = State

main :: IO ()
main = do
    --putStrLn "Start connect"
    b <- run startConnect
    --putStrLn "Get result"
    case b of
        Right _ -> do 
            --putStrLn "Get Right"
            _ <- G.init Nothing
            builderLogin <- G.builderNewFromFile $ pack "/mnt/c/Users/adnin/Documents/University/FP/project/social-studing-web/client/app/Login.glade"
            --putStrLn "Create builder"
            --G.builderAddFromFile builderLogin $ pack "/mnt/c/Users/adnin/Documents/University/FP/project/social-studing-web/client/app/Login.glade"
            --putStrLn "Add file to builder"
            (Just w1)    <- G.builderGetObject builderLogin "window1"  --loginWin
            (Just w2)    <- G.builderGetObject builderLogin "window2"  --mainWin
            (Just e1)    <- G.builderGetObject builderLogin "entry1"   --loginEntry
            (Just e2)    <- G.builderGetObject builderLogin "entry2"   --passEntry
            (Just btn1)  <- G.builderGetObject builderLogin "button1"  --loginBtn
            (Just btn2)  <- G.builderGetObject builderLogin "button2"  --generalBtn
            (Just btn3)  <- G.builderGetObject builderLogin "button3"  --studentBtn
            (Just btn4)  <- G.builderGetObject builderLogin "button4"  --lecturerBtn
            (Just btn5)  <- G.builderGetObject builderLogin "button5"  --adminBtn
            (Just bx1)   <- G.builderGetObject builderLogin "box1"     --generalBox
            (Just bx2)   <- G.builderGetObject builderLogin "box2"     --studentBox
            (Just bx3)   <- G.builderGetObject builderLogin "box3"     --lecturerBox
            (Just bx4)   <- G.builderGetObject builderLogin "box4"     --adminBox
            (Just l1)    <- G.builderGetObject builderLogin "label5"   --nameLabel
            (Just l2)    <- G.builderGetObject builderLogin "label7"   --infoLabel
            (Just l3)    <- G.builderGetObject builderLogin "label8"   --groupLabel
            (Just e3)    <- G.builderGetObject builderLogin "entry3"   --findEntry
            (Just btn6)  <- G.builderGetObject builderLogin "button6"  --findBtn
            (Just btn7)  <- G.builderGetObject builderLogin "button21" --addPostBtn
            (Just btn8)  <- G.builderGetObject builderLogin "button17" --addUserBtn
            (Just btn9)  <- G.builderGetObject builderLogin "button18" --delUserBtn
            (Just btn10) <- G.builderGetObject builderLogin "button19" --addStudentBtn
            (Just btn11) <- G.builderGetObject builderLogin "button20" --modStudentBtn
            (Just btn12) <- G.builderGetObject builderLogin "button7"  --downloadBtn
            (Just btn13) <- G.builderGetObject builderLogin "button8"  --downloadBtn
            (Just btn14) <- G.builderGetObject builderLogin "button9"  --downloadBtn
            (Just btn15) <- G.builderGetObject builderLogin "button10" --downloadBtn
            (Just btn16) <- G.builderGetObject builderLogin "button11" --downloadBtn
            (Just btn17) <- G.builderGetObject builderLogin "button12" --downloadBtn
            (Just btn18) <- G.builderGetObject builderLogin "button13" --downloadBtn
            (Just btn19) <- G.builderGetObject builderLogin "button14" --downloadBtn
            (Just btn20) <- G.builderGetObject builderLogin "button15" --downloadBtn
            (Just btn21) <- G.builderGetObject builderLogin "button16" --downloadBtn
            (Just bx5)   <- G.builderGetObject builderLogin "vbox5"    --vbox
            (Just bx6)   <- G.builderGetObject builderLogin "vbox6"    --vbox
            (Just bx7)   <- G.builderGetObject builderLogin "vbox7"    --vbox
            (Just bx8)   <- G.builderGetObject builderLogin "vbox8"    --vbox
            (Just bx9)   <- G.builderGetObject builderLogin "vbox9"    --vbox
            (Just bx10)  <- G.builderGetObject builderLogin "vbox10"   --vbox
            (Just bx11)  <- G.builderGetObject builderLogin "vbox11"   --vbox
            (Just bx12)  <- G.builderGetObject builderLogin "vbox12"   --vbox
            (Just bx13)  <- G.builderGetObject builderLogin "vbox13"   --vbox
            (Just bx14)  <- G.builderGetObject builderLogin "vbox14"   --vbox
            (Just l4)    <- G.builderGetObject builderLogin "label9"   --titleLabel
            (Just l5)    <- G.builderGetObject builderLogin "label11"  --titleLabel
            (Just l6)    <- G.builderGetObject builderLogin "label13"  --titleLabel
            (Just l7)    <- G.builderGetObject builderLogin "label15"  --titleLabel
            (Just l8)    <- G.builderGetObject builderLogin "label17"  --titleLabel
            (Just l9)    <- G.builderGetObject builderLogin "label19"  --titleLabel
            (Just l10)   <- G.builderGetObject builderLogin "label21"  --titleLabel
            (Just l11)   <- G.builderGetObject builderLogin "label23"  --titleLabel
            (Just l12)   <- G.builderGetObject builderLogin "label25"  --titleLabel
            (Just l13)   <- G.builderGetObject builderLogin "label27"  --titleLabel
            (Just l14)   <- G.builderGetObject builderLogin "label10"  --contentLabel
            (Just l15)   <- G.builderGetObject builderLogin "label12"  --contentLabel
            (Just l16)   <- G.builderGetObject builderLogin "label14"  --contentLabel
            (Just l17)   <- G.builderGetObject builderLogin "label16"  --contentLabel
            (Just l18)   <- G.builderGetObject builderLogin "label18"  --contentLabel
            (Just l19)   <- G.builderGetObject builderLogin "label20"  --contentLabel
            (Just l20)   <- G.builderGetObject builderLogin "label22"  --contentLabel
            (Just l21)   <- G.builderGetObject builderLogin "label24"  --contentLabel
            (Just l22)   <- G.builderGetObject builderLogin "label26"  --contentLabel
            (Just l23)   <- G.builderGetObject builderLogin "label28"  --contentLabel
            (Just w3)    <- G.builderGetObject builderLogin "window3"  --addUserWin
            (Just e4)    <- G.builderGetObject builderLogin "entry4"   --loginNewEntry
            (Just e5)    <- G.builderGetObject builderLogin "entry5"   --passNewEntry
            (Just ck1)   <- G.builderGetObject builderLogin "checkbutton1" --studentCkBtn
            (Just ck2)   <- G.builderGetObject builderLogin "checkbutton2" --lecturerCkBtn
            (Just ck3)   <- G.builderGetObject builderLogin "checkbutton3" --adminCkBtn
            --putStrLn "Find window1"
            window1  <- (G.unsafeCastTo G.Window) $ w1
            window2  <- (G.unsafeCastTo G.Window) $ w2
            entry1   <- (G.unsafeCastTo G.Entry)  $ e1
            entry2   <- (G.unsafeCastTo G.Entry)  $ e2
            button1  <- (G.unsafeCastTo G.Button) $ btn1
            button2  <- (G.unsafeCastTo G.Button) $ btn2
            button3  <- (G.unsafeCastTo G.Button) $ btn3
            button4  <- (G.unsafeCastTo G.Button) $ btn4
            button5  <- (G.unsafeCastTo G.Button) $ btn5
            box1     <- (G.unsafeCastTo G.Box)    $ bx1
            box2     <- (G.unsafeCastTo G.Box)    $ bx2
            box3     <- (G.unsafeCastTo G.Box)    $ bx3
            box4     <- (G.unsafeCastTo G.Box)    $ bx4
            label1   <- (G.unsafeCastTo G.Label)  $ l1
            label2   <- (G.unsafeCastTo G.Label)  $ l2
            label3   <- (G.unsafeCastTo G.Label)  $ l3
            entry3   <- (G.unsafeCastTo G.Entry)  $ e3
            button6  <- (G.unsafeCastTo G.Button) $ btn6
            button7  <- (G.unsafeCastTo G.Button) $ btn7
            button8  <- (G.unsafeCastTo G.Button) $ btn8
            button9  <- (G.unsafeCastTo G.Button) $ btn9
            button10 <- (G.unsafeCastTo G.Button) $ btn10
            button11 <- (G.unsafeCastTo G.Button) $ btn11
            buttons <- forM [btn12, btn13, btn14, btn15, btn16, btn17, btn18, btn19, btn20, btn21] (G.unsafeCastTo G.Button)
            tLabels <- forM [l4, l5, l6, l7, l8, l9, l10, l11, l12, l13] (G.unsafeCastTo G.Label)
            cLabels <- forM [l14, l15, l16, l17, l18, l19, l20, l21, l22, l23] (G.unsafeCastTo G.Label)
            vbxs <- forM [bx5, bx6, bx7, bx8, bx9, bx10, bx11, bx12, bx13, bx14] (G.unsafeCastTo G.Box)
            let gui = GUI window1 entry1 entry2 button1 window2 button2 button3 button4 button5 box1 label1 label2 box2 label3 entry3 button6 vbxs tLabels cLabels buttons box3 button7 box4 button8 button9 button10 button11
            G.onButtonClicked (loginBtn gui) $ login gui
            --putStrLn "Get window"
            G.onWidgetDestroy window1 G.mainQuit
            G.onWidgetDestroy window2 G.mainQuit
            --putStrLn "Set up window"
            G.widgetShowAll window1
            --putStrLn "Show window"
            G.main
        Left _ -> putStrLn "Problem..."

cfg = Config "http://localhost:3000"

run action = withState cfg $ \state -> flip runReaderT state $ action

login :: GUI -> IO ()
login gui = do
    loginBuffer <- G.entryGetBuffer . loginEntry $ gui
    login <- G.entryBufferGetText loginBuffer
    passBuffer <- G.entryGetBuffer . passEntry $ gui
    pass <- G.entryBufferGetText passBuffer
    user <- run $ loginConnect (login, pass)
    G.widgetHide $ loginWin gui
    G.onButtonClicked (generalBtn gui) $ general gui user
    G.onButtonClicked (studentBtn gui) $ student gui user
    G.onButtonClicked (lecturerBtn gui) $ lecturer gui user
    G.onButtonClicked (adminBtn gui) $ admin gui user
    G.onButtonClicked (findBtn gui) $ Main.find gui user
    G.widgetShowAll $ mainWin gui
    G.widgetHide $ studentBox gui
    G.widgetHide $ lecturerBox gui
    G.widgetHide $ adminBox gui

general :: GUI -> T.User -> IO ()
general gui user = do
    G.widgetHide $ studentBox gui
    G.widgetHide $ lecturerBox gui
    G.widgetHide $ adminBox gui
    G.labelSetText (nameLabel gui) $ pack (T.name user)
    G.labelSetText (infoLabel gui) $ "Администратор"
    G.widgetShowAll $ generalBox gui

student :: GUI -> T.User -> IO ()
student _ (T.User _ _ _ False _ _) = return ()
student gui user = do
    case T.student user of
        False -> return ()
        True -> do
            G.widgetHide $ generalBox gui
            G.widgetHide $ lecturerBox gui
            G.widgetHide $ adminBox gui
            student <- run $ getStudentConnect $ T.userId user
            G.labelSetText (groupLabel gui) $ pack . show $ student 
            posts <- run $ getPostsConnect $ T.userId user
            G.widgetShowAll $ studentBox gui

lecturer :: GUI -> T.User -> IO ()
lecturer gui user = do
    case T.lecturer user of
        False -> return ()
        True -> do
            G.widgetHide $ generalBox gui
            G.widgetHide $ studentBox gui
            G.widgetHide $ adminBox gui
            G.widgetShowAll $ lecturerBox gui

admin :: GUI -> T.User -> IO ()
admin _ (T.User _ _ _ _ _ False) = return ()
admin gui user = do
    G.widgetHide $ generalBox gui
    G.widgetHide $ studentBox gui
    G.widgetHide $ lecturerBox gui
    G.widgetShowAll $ adminBox gui
    
find :: GUI -> T.User -> IO ()
find _ (T.User _ _ _ False _ _) = return ()
find gui user = do
    buffer <- G.entryGetBuffer . findEntry $ gui
    find <- G.entryBufferGetText buffer
    posts <- run $ findPostsConnect (T.userId user) find
    G.widgetShowAll $ studentBox gui