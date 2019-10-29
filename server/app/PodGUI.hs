{-# LANGUAGE OverloadedLabels  #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE OverloadedLists   #-}

module PodGUI where
    import qualified GI.Gtk as Gtk
    {-import Data.GI.Base-}
    import GI.Gtk.Declarative
    import GI.Gtk.Declarative.App.Simple


    data GUI = GUI {
        mainWin :: Gtk.Window,
        mwAddBt :: Gtk.Button,
        mwUpdateBt :: Gtk.Button,
        mwDownloadBt :: Gtk.Button,
        mwFetchBt :: Gtk.Button,
        mwExitBt :: Gtk.Button,
        statusWin :: Gtk.Dialog,
        swOKBt :: Gtk.Button,
        swCancelBt :: Gtk.Button,
        swLabel :: Gtk.Label,
        addWin :: Gtk.Dialog,
        awOKBt :: Gtk.Button,
        awCancelBt :: Gtk.Button,
        awEntry :: Gtk.Entry
    }

    type State = ()
    
    data Event = Closed

    main :: IO ()
    main = run App {view = view', update = update', inputs = [], initialState = ()}

    view' :: State -> AppView Gtk.Window Event
    view' s = bin Gtk.Window [#title := "Demo", on #deleteEvent (const (True, Closed))] 
              $ widget Gtk.Label [#label := "Hello, World!"]
    
    update' :: State -> Event -> Transition State Event
    update' _ Closed = Exit
    
    