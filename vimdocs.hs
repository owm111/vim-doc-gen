module Main where

import Control.Exception
import Control.Monad
import Data.Char
import Data.List
import Data.List.Index
import Data.Ord
import System.Console.GetOpt
import System.Environment
import System.Exit
import System.IO
import Text.ParserCombinators.ReadP
import Text.Read (readMaybe)

main :: IO ()
main =
  getArgs >>= getOptM RequireOrder options >>= handleOpts >>= uncurry runCommand >>= putStrLn

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
parseAndFormat tw tagPad descPad parseLineF foldF tagPrefix prepareDesc =
  unlines . sort . map (formatF . prepareDesc) . foldF . map parseLineF . lines
  where formatF (x,y,z) = mkFormatter tw tagPad descPad tagPrefix x y z

-- * CLI stuff

runCommand :: CLIOpts -> [String] -> IO String
runCommand opts args
  | "help":_         <- args = return usage
  | "functions":rest <- args = go rest parseFunctionLine flToDescribed ""    dfToTriple
  | "commands":rest  <- args = go rest parseCommandLine  clToDescribed ":"   dcToTriple
  | "syntax":rest    <- args = go rest parseSyntaxLine   slToDescribed "hl-" dsToTriple
  | "section":rest   <- args = fmtSection opts rest
  | "subsection":rest<- args = fmtSubsection opts rest
  | "nosection":rest <- args = noSection opts rest
  | "modeline":rest  <- args = mkModeline opts rest
  | "header":rest    <- args = fmtHeader opts rest
  | "file":rest      <- args = runOnFile opts rest
  | otherwise                = ioError $ userError $ "Invalid command!\n" ++ usage
  where
        parseF :: Ord described => (String -> line) -> ([line] -> [described])
               -> String -> (described -> (String, String, String)) -> String -> String
        parseF = parseAndFormat (optsTermWidth opts) (optsTagPadding opts) (optsDescPadding opts)
        go args parse describe prefix triplify = do
          file <-
            case args of
             "-":_  -> getContents
             []     -> getContents
             path:_ -> readFile path
          return (parseF parse describe prefix triplify file)

getOptM :: ArgOrder a -> [OptDescr a] -> [String] -> IO ([a], [String])
getOptM argOrder optDescrs rawArgs
  | null errs = return (opts, args)
  | otherwise = ioError $ userError $ concat errs ++ usage
  where (opts, args, errs) = getOpt argOrder optDescrs rawArgs

handleOpts :: ([CLIOpts -> IO CLIOpts], [String]) -> IO (CLIOpts, [String])
handleOpts (optFs, args) = fmap (\x -> (x, args)) (parseOpts optFs)

parseOpts :: [CLIOpts -> IO CLIOpts] -> IO CLIOpts
parseOpts = foldM (\x f -> f x) defaultCLIOpts

data CLIOpts = CLIOpts
  { optsTermWidth :: Int
  , optsTagPadding :: Int
  , optsDescPadding :: Int
  } deriving Show

defaultCLIOpts = CLIOpts 78 3 30

usage = usageInfo usageHeader options

usageHeader = "Usage:\n  vim-doc-gen [-wtd] <command> [file]\nOptions:"

options :: [OptDescr (CLIOpts -> IO CLIOpts)]
options =
  [ Option ['w'] ["term-width"]
    (ReqArg (f readMaybe (\old val -> old { optsTermWidth = val   }) "chars must be an integer!") "chars")
    "Maximum width, like Vim's tw."
  , Option ['t'] ["tag-padding"]
    (ReqArg (f readMaybe (\old val -> old { optsTagPadding = val  }) "chars must be an integer!") "chars")
    "Amount of spaces between tags and the right margin."
  , Option ['d'] ["desc-padding"]
    (ReqArg (f readMaybe (\old val -> old { optsDescPadding = val }) "chars must be an integer!") "chars")
    "Amount descriptions should be indented."
  ]
  where f maybeF optF errMsg = \arg old -> maybe (ioError $ userError errMsg) (return . optF old) (maybeF arg)

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
    ' ':s -> this ++ "\n" ++ linesWidth width s
    _     -> reverse revBeforeSpace ++ "\n" ++ linesWidth width (reverse revAfterSpace ++ remaining)
  where (this, remaining) = splitAt width str
        (revAfterSpace, ' ':revBeforeSpace) = span (/= ' ') (reverse this)

-- * Format stuff

generalSection :: Char -> CLIOpts -> [String] -> IO String
generalSection c CLIOpts { optsTermWidth = width, optsTagPadding = pad } args =
  case args of
    [] -> ioError $ userError "Please two arguments!"
    [x] -> go (map toUpper x)
    _ -> go (map toUpper (concat (intersperse " " (init args))))
  where tag = "*" ++ last args ++ "*"
        go text = return $ replicate width c ++ "\n" ++ text ++ replicate (width - pad - length text - length tag) ' ' ++ tag ++ replicate pad ' '

fmtSection = generalSection '='
fmtSubsection = generalSection '-'

noSection = generalSection ' '

fmtHeader :: CLIOpts -> [String] -> IO String
fmtHeader CLIOpts {} args =
  case args of
    [fileName, version, date] -> return ("*" ++ fileName ++ "*\tFor Vim version " ++ version ++ "\tLast change: " ++ date)
    _ -> ioError $ userError "Requires exactly three arguments: file name, required version, and latst change date."

mkModeline :: CLIOpts -> [String] -> IO String
mkModeline CLIOpts { optsTermWidth = tw } _ = return ("vim:ft=help:norl:ts=8:tw=" ++ show tw ++ ":")

-- * File parser

data DocFileLine = DFNormal String | DFCommand [String] deriving Show

runOnFile :: CLIOpts -> [String] -> IO String
runOnFile opts args = do
  file <-
    case args of
      "-":_ -> getContents
      [] -> getContents
      file:_ -> readFile file
  parseDocFile opts file >>= (return . unlines)

parseDocFile :: CLIOpts -> String -> IO [String]
parseDocFile opts@CLIOpts { optsTermWidth = width } content = imapM f parsed
  where parsed = map (parseDfLine) (lines content)
        f :: Int -> DocFileLine -> IO String
        f _ (DFNormal x) = return (linesWidth width x)
        f i (DFCommand xs) = runCommand opts xs `catch` handler i
        handler :: Int -> IOException -> IO String  
        handler i e = ioError (userError ("on line " ++ show (succ i) ++ ": \n" ++ displayException e))

parseDfLine = dfCommandParser ?-> dfEscapedParser ?-> dfNormalParser ?=> DFNormal "stop"

dfCommandParser = do
  char '$'
  skipSpaces1
  argsStr <- many get
  eof
  return (DFCommand (splitArgs argsStr))

dfEscapedParser = do
  char '$'
  char '$'
  text <- many get
  eof
  return (DFNormal ('$':text))

dfNormalParser = do
  text <- many get
  eof
  return (DFNormal text)
  where emptyLine = eof >> return ""
        oneCharLine = do
          c <- satisfy (/= '$')
          eof
          return [c]
        escapedLine = do
          char '$'
          char '$'
          text <- many get
          return ('$':text)
        regularLine = many get

-- Borrowed from Cabal's Distribution.Simple.Setup
splitArgs :: String -> [String]
splitArgs = space []
  where space :: String -> String -> [String]
        space w [] = word w []
        space w (c:s) | isSpace c = word w (space [] s)
        space w ('"':s) = string w s
        space w s = nonstring w s
        string :: String -> String -> [String]
        string w []      = word w []
        string w ('"':s) = space w s
        string w ('\\':'"':s) = string ('"':w) s
        string w ( c :s) = string (c:w) s
        nonstring :: String -> String -> [String]
        nonstring w  []      = word w []
        nonstring w  ('"':s) = string w s
        nonstring w  ( c :s) = space (c:w) s
        word [] s = s
        word w  s = reverse w : s
