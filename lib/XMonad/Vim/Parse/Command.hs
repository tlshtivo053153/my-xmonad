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
import Data.List ( isPrefixOf, isInfixOf, find )

import Control.Arrow ( first )

type CompleteFunc = String -> String -> Bool
data Command = Command String CompleteFunc [Command]

parse :: [Command] -> String -> ([String], [String])
parse cs = fromMaybe ([],[]) . P.parse (parseCommands cs) 

parse' :: String -> [String]
parse' str = fromMaybe [] $ P.parse (sepBy1 escape (some $ char ' ')) str

parseCommands :: [Command] -> Parser ([String], [String])
parseCommands cs = do
    many $ char ' '
    (consumed, cs') <- commandMany cs
    comp <- escape
    let candidate = map commandName $ filter (\(Command name f _) -> f comp name) cs'
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
commandName (Command str _ _) = str

commandFunc :: Command -> CompleteFunc
commandFunc (Command _ f _) = f

commandArgs :: Command -> [Command]
commandArgs (Command _ _ args) = args

loopCommand :: CompleteFunc -> [String] -> [Command]
loopCommand f xs = [ Command x f (loopCommand f xs) | x <- xs ]

toCommand :: CompleteFunc -> [String] -> [Command]
toCommand f xs = [ Command x f [] | x <- xs ]

stringToEntry :: String -> String
stringToEntry = foldr f "" where
    f c str | c == ' ' ||
              c == '\"' = '\\':c:str
            | otherwise = c:str

ex1 :: [Command]
ex1 = [echo, exec]

echo :: Command
echo = Command "echo" isPrefixOf echoArgs

echoArgs :: [Command]
echoArgs = loopCommand isInfixOf [ "foo", "hello", "world", "hello!", "world!" ]

exec :: Command
exec = Command "exec" isPrefixOf execArgs

execArgs :: [Command]
execArgs = toCommand isInfixOf [ "LXDE", "Xfce4", "XMonad" ]

