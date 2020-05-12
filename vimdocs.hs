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
    "functions":_ -> getContents >>= (putStrLn . parseAndFormat parseFunctionLine flToDescribed formatDescribedFunction)
    "commands":_ -> getContents >>= (putStrLn . parseAndFormat parseCommandLine clToDescribed formatDescribedCommand)
    "syntax":_ -> getContents >>= (putStrLn . parseAndFormat parseSyntaxLine slToDescribed formatDescribedSyntax)
    _ -> hPutStrLn stderr "Wrong arguments!"

parseAndFormat
  :: (Ord described, Show described, Show line)
  => (String -> line) -> ([line] -> [described]) -> (described -> String) -> String -> String
parseAndFormat parseLineF foldF formatF file = unlines (sort (map formatF (foldF (map (parseLineF) (lines file)))))

-- * Functions

data FunctionsLine
  = FLEmpty
  | FLComment String
  | FLFunction String [String]
  deriving (Eq, Show)

data DescribedFunction
  = DescribedFunction
  { dfName :: String
  , dfArgs :: [String]
  , dfDesc :: String
  } deriving (Eq, Show)

instance Ord DescribedFunction where
  compare = comparing dfName

parseFunctionLine :: String -> FunctionsLine
parseFunctionLine x =
  case readP_to_S flFunctionParser x of
    [(flFunction, "")] -> flFunction
    _ ->
      case readP_to_S flCommentParser x of
        [(flComment, "")] -> flComment
        _ -> FLEmpty

flCommentParser = do
  char '"'
  skipSpaces
  comment <- many1 get
  eof
  return (FLComment comment)

flFunctionParser = do
  string "function "
  skipSpaces
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

flToDescribed :: [FunctionsLine] -> [DescribedFunction]
flToDescribed xs = fst $ foldl g ([],[]) xs
  where g (res, _       ) FLEmpty           = (res, [])
        g (res, comments) (FLComment c)     = (res, comments ++ [c])
        g (res, comments) (FLFunction f as) = (res ++ [DescribedFunction f as (unwords comments)], [])

formatDescribedFunction (DescribedFunction name args description) =
  tagSpacing ++ tag ++ "\n" ++ functionCall ++ callPadding ++ paddedDescriptionLines
  where tag = "*" ++ name ++ "*"
        tagSpacing = replicate (78 - 3 - length tag) ' '
        descriptionLines = linesWidth (78-30) description
        argsStr = formatArgs args
        functionCall = name ++ argsStr
        paddedDescriptionLines = insertAfterNewlines (replicate 30 ' ') descriptionLines
        callPadding =
          if length functionCall < 30
          then replicate (30 - length functionCall) ' '
          else "\n" ++ replicate 30 ' '

formatArgs [] = "()"
formatArgs [x] = "( {" ++ x ++ "} )"
formatArgs xs = "( " ++ concat (intersperse ", " (map (\x -> "{" ++ x ++ "}") xs)) ++ " )"

-- * Commands

data CommandsLine
  = CLEmpty
  | CLComment String
  | CLArguments String
  | CLCommand String
  deriving (Eq, Show)

data DescribedCommand
  = DescribedCommand
  { dcName :: String
  , dcArgs :: Maybe String
  , dcDesc :: String
  } deriving (Eq, Show)

instance Ord DescribedCommand where
  compare = comparing dcName

parseCommandLine :: String -> CommandsLine
parseCommandLine x =
  case readP_to_S clCommandParser x of
    [(clCommand, _)] -> clCommand
    _ ->
      case readP_to_S clArgumentsParser x of
        [(clArguments, "")] -> clArguments
        _ ->
          case readP_to_S clCommentParser x of
            [(clComment, "")] -> clComment
            _ -> CLEmpty

clCommentParser = do
  char '"'
  skipSpaces
  comment <- many1 get
  eof
  return (CLComment comment)

clArgumentsParser = do
  char '"'
  skipSpaces
  string "Arguments:"
  skipSpaces
  arguments <- many1 get
  eof
  return (CLArguments arguments)

clCommandParser = do
  string "command "
  skipSpaces
  many argParser
  comName <- nameParser
  void (char ' ') +++ eof
  return (CLCommand comName)
  where argParser = do
          char '-'
          arg <- many1 (satisfy (/= ' '))
          skipSpaces
          return arg
        nameParser = do
          first <- satisfy (`elem` ['A'..'Z'])
          rest <- many (satisfy (/= ' '))
          peek <- look
          return (first:rest)
        openParen = char '('
        closeParen = char ')'


clToDescribed :: [CommandsLine] -> [DescribedCommand]
clToDescribed xs = fst $ foldl g ([],[],Nothing) xs
  where g (res, _       , _   ) CLEmpty         = (res, [], Nothing)
        g (res, comments, _   ) (CLComment c)   = (res, comments ++ [c], Nothing)
        g (res, comments, _   ) (CLArguments a) = (res, comments, Just a)
        g (res, comments, args) (CLCommand c)   = (res ++ [DescribedCommand c args (unwords comments)], [], Nothing)
        fst (a,_,_) = a

formatDescribedCommand (DescribedCommand name args description) =
  tagSpacing ++ tag ++ "\n" ++ runCommand ++ callPadding ++ paddedDescriptionLines
  where tag = "*:" ++ name ++ "*"
        tagSpacing = replicate (78 - 3 - length tag) ' '
        descriptionLines = linesWidth (78-30) description
        runCommand = name ++ maybe "" (" " ++) args
        paddedDescriptionLines = insertAfterNewlines (replicate 30 ' ') descriptionLines
        callPadding =
          if length runCommand < 30
          then replicate (30 - length runCommand) ' '
          else "\n" ++ replicate 30 ' '

-- * Syntax

data SyntaxLine
  = SLEmpty
  | SLComment String
  | SLSyntax String
  deriving (Eq, Show)

data DescribedSyntax
  = DescribedSyntax
  { dsName :: String
  , dsDesc :: String
  } deriving (Eq, Show)

instance Ord DescribedSyntax where
  compare = comparing dsName

parseSyntaxLine :: String -> SyntaxLine
parseSyntaxLine x =
  case readP_to_S slSyntaxParser x of
    [(slSyntax, _)] -> slSyntax
    _ ->
      case readP_to_S slCommentParser x of
        [(slComment, "")] -> slComment
        _ -> SLEmpty

slCommentParser = do
  char '"'
  skipSpaces
  comment <- many1 get
  eof
  return (SLComment comment)

slSyntaxParser = do
  string "syntax "
  skipSpaces
  string "match" +++ string "keyword" +++ string "region"
  char ' '
  skipSpaces
  synName <- many1 (satisfy (/= ' '))
  void (char ' ') +++ eof
  return (SLSyntax synName)

slToDescribed :: [SyntaxLine] -> [DescribedSyntax]
slToDescribed xs = fst $ foldl g ([],[]) xs
  where g (res, _       ) SLEmpty           = (res, [])
        g (res, comments) (SLComment c)     = (res, comments ++ [c])
        g (res, comments) (SLSyntax s) = (res ++ [DescribedSyntax s (unwords comments)], [])

formatDescribedSyntax (DescribedSyntax name description) =
  tagSpacing ++ tag ++ "\n" ++ name ++ callPadding ++ paddedDescriptionLines
  where tag = "*hl-" ++ name ++ "*"
        tagSpacing = replicate (78 - 3 - length tag) ' '
        descriptionLines = linesWidth (78-30) description
        paddedDescriptionLines = insertAfterNewlines (replicate 30 ' ') descriptionLines
        callPadding =
          if length name < 30
          then replicate (30 - length name) ' '
          else "\n" ++ replicate 30 ' '

-- * Generic stuff

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
