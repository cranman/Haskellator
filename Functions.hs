module Functions(mymap, myfoldr, myfoldl, mysum, myproduct, listlength) where

type Function = String
type Parameter = String
type Accumulator = String
type Result = String

myhead :: String -> Result
myhead list = if ',' `elem` list
		     then takeWhile (/= ',') (tail list)
		     else takeWhile (/= ']') (tail list)

mytail :: String -> Result
mytail [] = "[]"
mytail list = if ',' `elem` list
		     then '[':(tail (dropWhile (/= ',') (tail list)))
		     else "[]"

mylength :: String -> Int
mylength [] = 0
mylength list = if ',' `elem` list
		      then 1 + (length $ filter (== ',') list)
		      else 1

myfoldl :: Function -> Accumulator -> Parameter -> Result
myfoldl func acc "[]"	= '\n':acc
myfoldl func acc list   = '\n':("foldl " ++ func ++ acc' ++ (mytail list) 
	++ (myfoldl func acc' (mytail list)))
		where
		acc' = " (" ++ func ++ ' ':acc ++ ' ':(myhead list) ++ ") "
			  
myfoldr :: Parameter -> Function -> Accumulator -> Parameter -> Result
myfoldr expr func acc "[]"	= '\n':expr ++ ' ':'(':acc ++ ")"
myfoldr expr func acc list	= '\n':expr ++ ' ':'(':func ++ ' ':(myhead list) ++ 
	" (foldr" ++ ' ':func ++ ' ':acc ++ ' ':(mytail list) ++ ") "  ++ 
	(myfoldr (expr ++ ' ':'(':func ++ ' ':(myhead list)) func acc (mytail list))
	++ ")"

mymap :: Parameter -> Function -> Parameter -> Result
mymap expr func "[]"	= '\n':expr ++ "[]"
mymap expr func list = '\n':expr ++ " (" ++ func ++ ' ':(myhead list) ++ 
	") : map" ++ ' ':func ++ ' ':(mytail list) ++ (mymap (expr ++ " (" ++ func
	++ ' ':(myhead list) ++ ") : ") func (mytail list))

mysum :: Parameter -> Result
mysum list = myfoldl "(+)" "0" list

myproduct :: Parameter -> Result
myproduct list = myfoldl "(*)" "1" list

listlength :: Parameter -> Result
listlength list = myfoldr "" "(\\x -> (+1))" "0" list