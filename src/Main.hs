{-# LANGUAGE TupleSections #-}

{-
    This file is part of Skef.

    Skef is free software: you can redistribute it and/or modify
    it under the terms of the GNU General Public License as published by
    the Free Software Foundation, either version 3 of the License, or
    any later version.

    Skef is distributed in the hope that it will be useful,
    but WITHOUT ANY WARRANTY; without even the implied warranty of
    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
    GNU General Public License for more details.

    You should have received a copy of the GNU General Public License
    along with Skef. If not, see <https://www.gnu.org/licenses/>.
-}

module Main where

import qualified System.IO as IO (readFile)

import qualified Data.Time.Calendar as Calendar
import qualified Data.Text as Text
import Debug.Trace (trace)
import Control.Monad (void)
import Data.Void
import Text.Megaparsec
import Text.Megaparsec.Char
import qualified Text.Megaparsec.Char.Lexer as Lex

import Types

type Parser = Parsec Void String

manySpaceNewline :: Parser ()
manySpaceNewline = void $ takeWhileP Nothing f
  where
    f x = x == ' ' || x == '\t' || x == '\n'

manySpace :: Parser ()
manySpace = void $ takeWhileP Nothing f
  where
    f x = x == ' ' || x == '\t'

someSpaceNewline :: Parser ()
someSpaceNewline = void $ takeWhile1P Nothing f
  where
    f x = x == ' ' || x == '\t' || x == '\n'

someSpace :: Parser ()
someSpace = void $ takeWhile1P Nothing f
  where
    f x = x == ' ' || x == '\t' || x == '\n'

guardGt :: MonadParsec e s m => Pos -> m Pos
guardGt pos = Lex.indentGuard (return ()) GT pos 

guardEq :: MonadParsec e s m => Pos -> m Pos
guardEq pos = Lex.indentGuard (return ()) EQ pos

pTextNode :: Pos -> Parser (Node)
pTextNode parentPos = do
    t <- some line
    return $ TextNode $ Text.pack $ unlines t
  where
    line = do
        _ <- guardGt parentPos
        t <- takeWhileP Nothing (\x -> x /= '\n' && x /= '[')
        someSpaceNewline
        return t

pLabel :: Pos -> Parser (Node)
pLabel parentPos = do
    _ <- guardGt parentPos
    pos <- Lex.indentLevel
    _ <- single '['
    t <- takeWhileP Nothing (\x -> x /= ']')
    _ <- single ']'
    _ <- manySpaceNewline
    children <- many (pLabel pos <|> pTextNode pos)
    return $ Custom (Text.pack t) (Text.pack "") children

pDateLabel :: Parser Calendar.Day
pDateLabel = do
        _ <- takeWhileP Nothing (\x -> x /= '-')
        _ <- single '-'
        _ <- manySpace
        year <- count 4 (digitChar <?> "year")
        _ <- single '-'
        month <- count 2 (digitChar <?> "month")
        _ <- single '-' 
        day <- count 2 (digitChar <?> "day")
        return (
          Calendar.fromGregorian 
            (read year::Integer)
            (read month::Int)
            (read day::Int)
          )

pDay :: Parser (Calendar.Day, [Node])
pDay = do
    pos <- Lex.indentLevel
    _ <- single '['
    d <- pDateLabel
    _ <- single ']'
    manySpaceNewline
    nodes <- many $ (pLabel pos <|> pTextNode pos)
    return (d, nodes)

parser :: Parser [(Calendar.Day, [Node])]
parser = many pDay <* eof

main :: IO ()
main = do
    input <- IO.readFile "tests/test.skef"
    output <- parseTest parser input
    print output
