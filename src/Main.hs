import System.IO
import System.Environment (getArgs)
import Control.Exception ( throw, Exception )
import Control.Monad ( when, unless )
--import Data.Char (isSpace)
import Data.List ( intercalate )
import Text.Read

-- Error handling
import Data.Typeable ( Typeable )
newtype Error = Error {errMsg :: String}
    deriving (Show, Typeable)
instance Exception Error

{- Didnt need it afterall
-- modified from https://stackoverflow.com/a/9722949
mapTriple :: (a -> b) -> (a, a, a) -> (b, b, b)
mapTriple f (a1, a2, a3) = (f a1, f a2, f a3)
-}

{- unneeded too lol
-- modified from https://stackoverflow.com/a/20124026
isWhitespace :: String -> Bool
isWhitespace = all isSpace
-}

-- Operations for triples
fst3 :: (a,b,c) -> a
fst3 (x,_,_) = x
snd3 :: (a,b,c) -> b
snd3 (_,y,_) = y
trd3 :: (a,b,c) -> c
trd3 (_,_,z) = z

helpMessage :: String
helpMessage = "hai"

listToPrint :: [String] -> String
listToPrint = intercalate "\n" 

possibleFlags :: String
possibleFlags = "h"
possibleLFlags :: [String]
possibleLFlags = ["sum", "names", "prices", "links"]

-- triple: (name, cost, link)

-- If a triple has a null first element, it does not have a name
hasName :: (String, String, String) -> Bool
hasName (name, _, _) = (not . null) name

-- Deletes all invalid triples from list
deleteEmpty :: [(String, String, String)] -> [(String, String, String)]
deleteEmpty = filter hasName

-- Removes duplicate entries from a list
removeDups :: forall a. Eq a => [a] -> [a]
removeDups []       = []
removeDups (x:xs)   = x : removeDups (filter (/= x) xs)

-- Trims one element off the left of a list
trimL :: [a] -> [a]
trimL []        = []
trimL (x:xs)    = xs

-- Trims one element off the right of a list 
trimR :: [a] -> [a]
trimR l = reverse (helper [] l)
   where
      helper acc lis = case lis of
         [x]        -> acc
         (x:xs)     -> helper (x:acc) xs
         _          -> acc

-- Trims the left and right elements off a list 
trimLR :: [a] -> [a]
trimLR = trimR . trimL

-- Ensure no invalid flags are passed
checkFlags :: [Char] -> Bool
checkFlags ""       = True
checkFlags (f:fs)   = f `elem` possibleFlags && checkFlags fs

-- I don't wanna use foldr here, it hurts my teeny brain when I look at it
checkLFlags :: [String] -> Bool
checkLFlags []          = True
checkLFlags (lf:lfs)    = lf `elem` possibleLFlags && checkLFlags lfs

isFlag :: String -> Bool
isFlag str = head str == '-'

removeDash :: String -> String
removeDash str = case str of
    (_:cs) -> cs

isLongFlag :: String -> Bool
isLongFlag str = case str of
    (c1:c2:cs) -> [c1, c2] == "--"
    _ -> False

remove2Dash :: String -> String
remove2Dash str = case str of
    (_:_:cs) -> cs

-- Parse the input to the program into argv, flags, and longflags
parseArgs :: [String] -> ([String], String, [String])
parseArgs []            = throw (Error "Error: No arguments specified")
parseArgs strs = helper strs [] "" []
    where
        helper args argv flags longFlags = case args of
            (a:as)
                | isLongFlag a  -> helper as  argv            flags                   (longFlags ++ [remove2Dash a])
                | isFlag a      -> helper as  argv           (flags ++ removeDash a)   longFlags
                | otherwise     -> helper as (argv ++ [a])    flags                    longFlags
            []                  -> (argv, flags, longFlags)

-- Ignore everything between /* */
handleMultilines :: String -> String
handleMultilines text = reverse (helper False [] text)
    where
        helper isComment acc str = case str of
            (c1:c2:cs)
                | isComment && [c1,c2] == "*/"  -> helper False acc cs
                | isComment                     -> helper True acc (c2:cs)
                | [c1,c2] == "/*"               -> helper True acc cs
                | otherwise                     -> helper False (c1:acc) (c2:cs) -- only iterate one character at a time, despite looking at 2
            (c:cs)
                | isComment                     -> helper True acc cs
                | otherwise                     -> helper False (c:acc) cs
            []                                  -> acc

-- Ignore anything after a #
handleComments :: String -> String
handleComments [] = ""
handleComments line = reverse (helper [] line)
    where
        helper acc str = case str of
            (c:cs)
                | c == '#'         -> acc
                | otherwise         -> helper (c:acc) cs
            []                      -> acc

-- Parse each line into a list of strings, handling inline comments
parseRaw :: String -> [String]
parseRaw text = reverse (helper [] (lines text))
    where
        helper acc [] = acc
        helper acc (str:strs) = helper (handleComments str:acc) strs

-- Splits a string into a tuple at the given delimiter character
splitHalfAt :: Char -> String -> (String, String)
splitHalfAt c str = helper c ("", str)
    where
        helper delim (s1, s2) = case s2 of
            []         -> (reverse s1, s2)
            (c:cs)
                | c == delim -> (reverse s1, cs)
                | otherwise -> helper delim (c:s1, cs)
            
-- Takes the first two elements of a list and turns them into a tuple
listToTuple :: [String] -> (String, String)
listToTuple []          = ("null", "null")
listToTuple [s1]        = (s1, "null")
listToTuple (s1:s2:_)   = (s1, s2)

-- Parse a line into a triple, with default of ""
parseLine :: String -> (String, String, String)
parseLine line = 
    let (s1, sx) = splitHalfAt ':' line in
    let (s2, s3) = listToTuple (words sx) in
    (s1, s2, s3)

{-
removeLastColon :: String -> String
removeLastColon []          = []
removeLastColon [c, ':']    = [c]
removeLastColon (c:cs)      = c:(removeLastColon cs)
-}

stripParens :: String -> String
stripParens [] = []
stripParens ('(':str)    = case reverse str of
    (')':str') -> reverse str'
    str -> str
stripParens str          = str

extractFloat :: String -> Float
extractFloat str = case readMaybe str :: Maybe Float of
    Just number -> number
    Nothing -> throw (Error ("Error: Malformed Number: " ++ str))

processNames :: [(String, String, String)] -> [String]
processNames = map fst3

processPrices :: [(String, String, String)] -> [String]
processPrices = map (helper . snd3)
    where
        helper ('$':ss)  = ss
        helper str       = str

sumPrices :: [String] -> Float
sumPrices strs = sum (map extractFloat strs)

processLinks :: [(String, String, String)] -> [String]
processLinks = map (stripParens . trd3)

main :: IO ()
main = do
    args <- getArgs
    let (argv, flags, longFlags) = parseArgs args

    if 'h' `elem` flags then do
        putStrLn helpMessage
    else do
        when (null argv)                $ throw (Error "Error: No arguments specified")
        unless (checkFlags flags)       $ throw (Error "Error: Invalid flag")
        unless (checkLFlags longFlags)  $ throw (Error "Error: Invalid long flag")

        let (filePath:arguments) = argv
        
        {-
        print ("filePath: " ++ filePath)
        print ("arguments: " ++ concat arguments)
        print ("flags: " ++ flags)
        print ("longFlags: " ++ concat longFlags)
        -}
        
        rawText <- readFile filePath
        
        -- Handle multiline comments
        let parse1 = handleMultilines rawText

        -- Handle inline comments and separate lines into a list
        let strList = parseRaw parse1

        -- Parse each line into triples, dropping the rest. Also remove invalid triples.
        let trimmedList = deleteEmpty (map parseLine strList)
        
        if "sum" `elem` longFlags then do
            print ((sumPrices . processPrices) trimmedList)
        else if "names" `elem` longFlags then do
            putStrLn (listToPrint (processNames trimmedList))
        else if "prices" `elem` longFlags then do
            putStrLn (listToPrint (processPrices trimmedList))
        else if "links" `elem` longFlags then do
            putStrLn (listToPrint (processLinks trimmedList))
        else do
            throw (Error "No arguments specified")
