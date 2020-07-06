module XMonad.Vim.Parse.Command
( Command(..)
, parse
, parse'
, parseCommands
, loopCommand
, toCommand
, stringToEntry
) where

import qualified Text.Appar.String as P
import Text.Appar.String hiding ( parse )

import Data.Maybe ( fromMaybe )
import Data.List ( isPrefixOf, find )

import Control.Arrow ( first )

data Command = Command String [Command]

parse :: [Command] -> String -> ([String], [String])
parse cs = fromMaybe ([],[]) . P.parse (parseCommands cs) 

parse' :: String -> [String]
parse' str = fromMaybe [] $ P.parse (sepBy1 escape (some $ char ' ')) str

parseCommands :: [Command] -> Parser ([String], [String])
parseCommands cs = do
    many $ char ' '
    (consumed, cs') <- commandMany cs
    comp <- escape
    let candidate = map commandName $ filter ( isPrefixOf comp . commandName) cs'
    return (consumed, candidate)

command :: [Command] -> Parser (String, [Command])
command commands= do
    c <- escape <* some (char ' ')
    let complete = commandArgs <$> find ((== c) . commandName) commands
        in return (c, fromMaybe [] complete)

commandMany :: [Command] -> Parser ([String], [Command])
commandMany cs = do
    c <- Just <$> try (command cs) <|> return Nothing
    case c of
        Just (str, c') -> first (str:) <$> commandMany c'
        Nothing        -> return ([], cs)

escape :: Parser String
escape = 
      char '\"' *>
      manyTill (escapeOf "\"") (char '\"')
  <|> many (escapeOf " ")

escapeOf :: String -> Parser Char
escapeOf str = char '\\' *> anyChar <|> noneOf str

commandName :: Command -> String
commandName (Command str _) = str

commandArgs :: Command -> [Command]
commandArgs (Command _ args) = args

loopCommand :: [String] -> [Command]
loopCommand xs = [ Command x (loopCommand xs) | x <- xs ]

toCommand :: [String] -> [Command]
toCommand xs = [ Command x [] | x <- xs ]

stringToEntry :: String -> String
stringToEntry = foldr f "" where
    f c str | c == ' ' ||
              c == '\"' = '\\':c:str
            | otherwise = c:str

ex1 :: [Command]
ex1 = [echo, exec]

echo :: Command
echo = Command "echo" echoArgs

echoArgs :: [Command]
echoArgs = loopCommand [ "foo", "hello", "world", "hello!", "world!" ]

exec :: Command
exec = Command "exec" execArgs

execArgs :: [Command]
execArgs = toCommand [ "LXDE", "Xfce4", "XMonad" ]

