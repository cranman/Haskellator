module Process where

import qualified Language.Haskell.Interpreter as Hint
import Control.Monad.Trans
import Graphics.UI.Gtk
import Text.Regex.Posix
import Functions
import Graphics.UI.Gtk.SourceView

updateText entryBox textView lang = do
      input <- get entryBox entryText
      r <- Hint.runInterpreter $ run (parseInput input) textView lang
      case r of
		Left err -> printInterpreterError err
		Right () -> return ()
      putStrLn input
      
-- Error returned to user
tempfunc textView = do
	tt <- textTagTableNew
	tb <- textBufferNew (Just tt)
	textBufferSetText tb "Please enter a valid Haskell expression"
	textViewSetBuffer textView tb
	return ()
  
-- function to run interpreter
run str temp lang = do
  Hint.setImportsQ [("Prelude", Nothing), ("Data.Map", Just "Map"),
	("Data.Maybe", Just "Maybe"), ("Data.List", Just "List"), 
	("Data.Char", Just "Char")]
  
  -- deal with runtime options
  case str of
	(":t", code)	-> do
						a <- Hint.typeOf code
						tt <- liftIO $ textTagTableNew
						tb <- Hint.liftIO $ sourceBufferNew (Just tt)
						Hint.liftIO $ textBufferSetText tb $ 
							code ++ '\n':'\n':a
						langs <- Hint.liftIO $ 
							sourceLanguageManagerGetLanguageIds lang
						Hint.liftIO $ print langs 
						language <- Hint.liftIO $ 
							sourceLanguageManagerGetLanguage lang "haskellator"
						Hint.liftIO $ sourceBufferSetLanguage tb language
						Hint.liftIO $ sourceBufferSetHighlightSyntax tb True
						Hint.liftIO $ textViewSetBuffer temp tb
	(":e", code)	-> do
						let parsed = getAllTextMatches $ parseUserInput code
						a <- Hint.eval code
						b <- Hint.typeOf code
						Hint.liftIO $ print parsed
						let result = case head parsed of
							"foldr" 	-> myfoldr "" (parsed !! 1) 
											(parsed !! 2) (parsed !! 3)
							"foldl"		-> myfoldl (parsed !! 1) 
											(parsed !! 2) (parsed !! 3)
							"map"		-> mymap "" (parsed !! 1) (parsed !! 2)
							"sum"		-> mysum (parsed !! 1)
							"product"	-> myproduct (parsed !! 1)
							"length"	-> listlength (parsed !! 1)
							_ -> concat parsed
						tt <- Hint.liftIO $ textTagTableNew
						tb <- Hint.liftIO $ sourceBufferNew (Just tt)
						Hint.liftIO $ textBufferSetText tb $ 
							code ++ '\n':'\n':b ++ '\n':(result) ++ '\n':'\n':a
						langs <- Hint.liftIO $ 
							sourceLanguageManagerGetLanguageIds lang
						Hint.liftIO $ print langs 
						language <- Hint.liftIO $ 
							sourceLanguageManagerGetLanguage lang "haskellator"
						Hint.liftIO $ sourceBufferSetLanguage tb language
						Hint.liftIO $ sourceBufferSetHighlightSyntax tb True
						Hint.liftIO $ textViewSetBuffer temp tb
						Hint.liftIO $ putStrLn(result)
	(_, code)		-> do
						a <- Hint.eval code
						b <- Hint.typeOf code
						tt <- Hint.liftIO $ textTagTableNew
						tb <- Hint.liftIO $ sourceBufferNew (Just tt)
						Hint.liftIO $ textBufferSetText tb $ 
							code ++ '\n':'\n':b ++ '\n':'\n':a
						langs <- Hint.liftIO $ 
							sourceLanguageManagerGetLanguageIds lang
						Hint.liftIO $ print langs 
						language <- Hint.liftIO $ 
							sourceLanguageManagerGetLanguage lang "haskellator"
						Hint.liftIO $ sourceBufferSetLanguage tb language
						Hint.liftIO $ sourceBufferSetHighlightSyntax tb True
						Hint.liftIO $ textViewSetBuffer temp tb
  
parseInput :: String -> (String ,String)
parseInput [] = ("", [])
parseInput str = case take 2 str of
					  ":t"		-> (":t", drop 3 str) -- Just print type to GUI
					  ":e"		-> (":e", drop 3 str) -- special evaluate
					  _			-> ("", str) -- just normal ghci-like 
					

parseUserInput :: String -> AllTextMatches [] String
parseUserInput str = str =~ "\\(.*?\\)|[^ ]+"
-- Match  '(' followed by characters and a ) OR ' ' multiple times

say :: String -> Hint.Interpreter ()
say = Hint.liftIO . putStrLn

write :: FilePath -> String -> Hint.Interpreter ()
write a b = Hint.liftIO (writeFile a (b ++ "\n"))

printInterpreterError :: Hint.InterpreterError -> IO ()
printInterpreterError e = putStrLn $ "Ups... " ++ (show e)