module Main where

import qualified Language.Haskell.Interpreter as Hint
import Control.Monad
import Graphics.UI.Gtk
import Graphics.UI.Gtk.Builder
import Graphics.UI.Gtk.Selectors.FileChooser
import Graphics.UI.Gtk.SourceView
import System.Glib.GError
import System.Exit
import System.Process
import Process
                       
main = do
    initGUI
    
    -- Load the .glade file
    builder 	<- builderNew
    handleGError (\(GError dom code msg) -> fail msg) $ 
		builderAddFromFile builder "haskellator.glade"
    
    -- Parse all the widgets used
    window      <- builderGetObject builder castToWindow "project"
    
    entry       <- builderGetObject builder castToEntry "entry1"
    
    --text    	<- builderGetObject builder castToTextView "textview1"
    text    	<- builderGetObject builder castToSourceView "gtksourceview1"
    
    menu 	<- builderGetObject builder castToMenuBar "menubar1"      
    file	<- builderGetObject builder castToMenuItem "menuitem1"	
    quit	<- builderGetObject builder castToMenuItem "imagemenuitem5"   	
    edit	<- builderGetObject builder castToMenuItem "menuitem2"  	
    view	<- builderGetObject builder castToMenuItem "menuitem3"	
    help	<- builderGetObject builder castToMenuItem "menuitem4"
	
    applyButton <- builderGetObject builder castToButton "button1"
    
    -- Syntax highlighting
    lang <- sourceLanguageManagerNew
    sourceLanguageManagerSetSearchPath lang (Just ["."])
    path <- sourceLanguageManagerGetSearchPath lang
    mapM_ print path
    
    -- Provide functionality to the widgets
    onActivateLeaf file $ do
      putStrLn "FILE"
	
    onActivateLeaf quit $ do
      mainQuit
      putStrLn "QUIT"

    onActivateLeaf help $ do
      putStrLn "HELP"
      
    onActivateLeaf view $ do
      putStrLn "VIEW"
      
    onActivateLeaf edit $ do
      putStrLn "EDIT"
    
    onDestroy window $ do
      mainQuit
    
    onClicked applyButton $ do
      updateText entry text lang
        
    afterEntryActivate entry $ do
      updateText entry text lang
	
    -- Show all the widgets    
    widgetShowAll window
    mainGUI
