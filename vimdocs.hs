module Main where

import Control.Monad
import Data.List
import Data.Ord
import System.Environment
import System.IO
import Text.ParserCombinators.ReadP

main :: IO ()
main = do
  args <- getArgs
  case args of 
    "functions":_ -> go parseFunctionLine flToDescribed "" dfToTriple
    "commands":_  -> go parseCommandLine clToDescribed ":" dcToTriple
    "syntax":_    -> go parseSyntaxLine slToDescribed "hl-" dsToTriple
    _             -> hPutStrLn stderr "Wrong arguments!"
  where go parse describe prefix triplify = getContents >>= (putStrLn . parseAndFormat 78 3 30 parse describe prefix triplify)

parseAndFormat
  :: Ord described
  => Int
  -> Int
  -> Int
  -> (String -> line)
  -> ([line] -> [described])
  -> String
  -> (described -> (String, String, String))
  -> String
  -> String
parseAndFormat tw tagPad descPad parseLineF foldF tagPrefix prepareDesc file =
  unlines (sort (map (formatF . prepareDesc) (foldF (map (parseLineF) (lines file)))))
  where formatF (x,y,z) = mkFormatter tw tagPad descPad tagPrefix x y z

-- * Types

-- ** Lines

data FunctionsLine
  = FLEmpty
  | FLComment String
  | FLFunction String [String]
  deriving (Eq, Show)

data CommandsLine
  = CLEmpty
  | CLComment String
  | CLArguments String
  | CLCommand String
  deriving (Eq, Show)

data SyntaxLine
  = SLEmpty
  | SLComment String
  | SLSyntax String
  deriving (Eq, Show)

-- ** Described

data DescribedFunction
  = DescribedFunction
  { dfName :: String
  , dfArgs :: [String]
  , dfDesc :: String
  } deriving (Eq, Show)

data DescribedCommand
  = DescribedCommand
  { dcName :: String
  , dcArgs :: Maybe String
  , dcDesc :: String
  } deriving (Eq, Show)

data DescribedSyntax
  = DescribedSyntax
  { dsName :: String
  , dsDesc :: String
  } deriving (Eq, Show)

instance Ord DescribedFunction where compare = comparing dfName
instance Ord DescribedCommand where compare = comparing dcName
instance Ord DescribedSyntax where compare = comparing dsName

-- * Parsers

simpleCommentParser :: (String -> a) -> ReadP a
simpleCommentParser f = do
  char '"'
  skipSpaces
  comment <- many1 get
  eof
  return (f comment)

flCommentParser :: ReadP FunctionsLine
flCommentParser = simpleCommentParser FLComment

clCommentParser :: ReadP CommandsLine
clCommentParser = simpleCommentParser CLComment

slCommentParser :: ReadP SyntaxLine
slCommentParser = simpleCommentParser SLComment

flFunctionParser :: ReadP FunctionsLine
flFunctionParser = do
  string "function"
  skipSpaces1
  fnName <- many1 (satisfy (`notElem` " ("))
  skipSpaces
  openParen
  fnArgs <- argParser `sepBy` (char ',')
  closeParen
  eof
  return (FLFunction fnName fnArgs)
  where argParser = do
          skipSpaces
          argName <- many1 (satisfy (`notElem` ",)"))
          skipSpaces
          return argName
        openParen = char '('
        closeParen = char ')'

clArgumentsParser :: ReadP CommandsLine
clArgumentsParser = do
  char '"'
  skipSpaces
  string "Arguments:"
  skipSpaces1
  arguments <- many1 get
  eof
  return (CLArguments arguments)

clCommandParser :: ReadP CommandsLine
clCommandParser = do
  string "command "
  skipSpaces
  many argParser
  comName <- nameParser
  spaceOrEof
  return (CLCommand comName)
  where argParser = do
          char '-'
          arg <- many1 (satisfy (/= ' '))
          skipSpaces
          return arg
        nameParser = do
          first <- satisfy (`elem` ['A'..'Z'])
          rest <- many (satisfy (/= ' '))
          return (first:rest)

slSyntaxParser :: ReadP SyntaxLine
slSyntaxParser = do
  string "syntax"
  skipSpaces1
  string "match" +++ string "keyword" +++ string "region"
  skipSpaces1
  synName <- many1 (satisfy (/= ' '))
  spaceOrEof
  return (SLSyntax synName)

skipSpaces1 :: ReadP ()
skipSpaces1 = char ' ' >> skipSpaces

spaceOrEof :: ReadP ()
spaceOrEof = void (char ' ') +++ eof

-- * Parse functions

infixr 1 ?->
(?->) ::  ReadP a -> (String -> a) -> String -> a
x ?-> y = \ s ->
  case readP_to_S x s of
    [(z,_)] -> z
    _ -> y s

infixr 1 ?=>
(?=>) ::  ReadP a -> a -> String -> a
x ?=> y = x ?-> const y

parseFunctionLine :: String -> FunctionsLine
parseFunctionLine = flFunctionParser ?-> flCommentParser ?=> FLEmpty

parseCommandLine :: String -> CommandsLine
parseCommandLine = clCommandParser ?-> clArgumentsParser ?-> clCommentParser ?=> CLEmpty

parseSyntaxLine :: String -> SyntaxLine
parseSyntaxLine = slSyntaxParser ?-> slCommentParser ?=> SLEmpty

-- * Line to described functions


flToDescribed :: [FunctionsLine] -> [DescribedFunction]
flToDescribed xs = fst $ foldl f ([],[]) xs
  where f (res, _       ) FLEmpty           = (res, [])
        f (res, comments) (FLComment c)     = (res, comments ++ [c])
        f (res, comments) (FLFunction f as) = (res ++ [DescribedFunction f as (unwords comments)], [])

clToDescribed :: [CommandsLine] -> [DescribedCommand]
clToDescribed xs = fst $ foldl f ([],[],Nothing) xs
  where f (res, _       , _   ) CLEmpty         = (res, [], Nothing)
        f (res, comments, _   ) (CLComment c)   = (res, comments ++ [c], Nothing)
        f (res, comments, _   ) (CLArguments a) = (res, comments, Just a)
        f (res, comments, args) (CLCommand c)   = (res ++ [DescribedCommand c args (unwords comments)], [], Nothing)
        fst (a,_,_) = a

slToDescribed :: [SyntaxLine] -> [DescribedSyntax]
slToDescribed xs = fst $ foldl f ([],[]) xs
  where f (res, _       ) SLEmpty       = (res, [])
        f (res, comments) (SLComment c) = (res, comments ++ [c])
        f (res, comments) (SLSyntax s)  = (res ++ [DescribedSyntax s (unwords comments)], [])

-- * Formatters

-- ** Specific

dfToTriple :: DescribedFunction -> (String, String, String)
dfToTriple (DescribedFunction name args desc) = (name, name ++ formatFunctionArgs args, desc)

formatFunctionArgs :: [String] -> String
formatFunctionArgs [] = "()"
formatFunctionArgs [x] = "( {" ++ x ++ "} )"
formatFunctionArgs xs = "( " ++ concat (intersperse ", " (map (\x -> "{" ++ x ++ "}") xs)) ++ " )"

dcToTriple :: DescribedCommand -> (String, String, String)
dcToTriple (DescribedCommand name args desc) = (name, name ++ maybe "" (' ':) args, desc)

dsToTriple :: DescribedSyntax -> (String, String, String)
dsToTriple (DescribedSyntax name desc) = (name, name, desc)

-- ** General

mkFormatter :: Int -> Int -> Int -> String -> String -> String -> String -> String
mkFormatter width tagPad descPad tagPrefix tag name desc =
  rightAlign width tagPad (mkTag tagPrefix tag) ++ "\n" ++ formatDescription width descPad name desc

mkTag :: String -> String -> String
mkTag prefix name = "*" ++ prefix ++ name ++ "*"

rightAlign :: Int -> Int -> String -> String
rightAlign width rPadding str = spaces lPadding ++ str ++ spaces rPadding
  where lPadding = width - rPadding - length str
        spaces = flip replicate ' '

formatDescription :: Int -> Int -> String -> String -> String
formatDescription width descPadding name desc = name ++ fstLinePadding ++ paddedDesc
  where fstLinePadding =
          if length name < descPadding
          then replicate (descPadding - length name) ' '
          else "\n" ++ replicate descPadding ' '
        paddedDesc = insertAfterNewlines (replicate descPadding ' ') linedDesc
        linedDesc = linesWidth (width - descPadding) desc

insertAfterNewlines _ [] = []
insertAfterNewlines insertme ('\n':str) = '\n' : (insertme ++ insertAfterNewlines insertme str)
insertAfterNewlines insertme (x:str) = x : insertAfterNewlines insertme str

linesWidth width str =
  case remaining of
    []    -> this
    ' ':_ -> this ++ "\n" ++ linesWidth width remaining
    _     -> reverse revBeforeSpace ++ "\n" ++ linesWidth width (reverse revAfterSpace ++ remaining)
  where (this, remaining) = splitAt width str
        (revAfterSpace, ' ':revBeforeSpace) = span (/= ' ') (reverse this)
