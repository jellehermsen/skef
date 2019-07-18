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

module Parser (Parser.parse) where

import qualified Data.Time.Calendar as Calendar
import qualified Data.Text as Text
import Data.Either (isLeft, isRight, fromLeft, fromRight)
import Debug.Trace (trace)
import Control.Monad (void)
import Data.Void
import Text.Megaparsec
import Text.Megaparsec.Char (digitChar)
import qualified Text.Megaparsec.Char.Lexer as Lex

import Types

type Parser = Parsec Void Text.Text

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
    return $ TextNode $ Text.unlines t
  where
    line = do
        _ <- guardGt parentPos
        t <- takeWhileP Nothing (\x -> x /= '\n' && x /= '[')
        someSpaceNewline
        return t

pCodeText :: Pos -> Parser (Text.Text)
pCodeText parentPos = do
    t <- some line
    return $ Text.unlines t
  where
    line = do
        _ <- guardGt parentPos
        t <- takeWhileP Nothing (/= '\n')
        -- TODO: preserve indentation inside code blocks
        someSpaceNewline
        return t


{-
data Node = Project Text.Text [Node] 
    | Code Text.Text
    | Todo Text.Text [Node]
    | Done Text.Text [Node]
    | Time Int Text.Text [Node]
    | TextNode Text.Text
    | Custom Text.Text Text.Text [Node]
  deriving (Show)
-}

node :: Text.Text -> Text.Text -> [Node] -> Either Text.Text Node
node "todo" descr children = Right $ Todo descr children
node "done" descr children = Right $ Done descr children
node "time" descr children = Right $ Time 0 timeSegment children
    where
        timeSegment = head $ Text.split (==' ') $ Text.strip descr

node labelName descr children = Right $ Custom labelName descr children

pLabel :: Pos -> Parser (Node)
pLabel parentPos = do
    _ <- guardGt parentPos
    pos <- Lex.indentLevel
    _ <- single '['
    t <- takeWhile1P Nothing (/= ']')
    _ <- single ']'
    _ <- manySpaceNewline
    let (labelName, remainder) = Text.breakOn " " $ Text.toLower $ Text.strip t
    if labelName /= "code" then do
        if labelName == "error" then fail "BOEMSHAKALAKA"
        else do
            children <- many (pLabel pos <|> pTextNode pos)
            let n = node labelName (Text.strip remainder) children
            if (isRight n) then
                return $ fromRight n
            else
                fail $ fromLeft n
    else do
        child <- pCodeText parentPos
        return $ Code child

pDateLabel :: Parser Calendar.Day
pDateLabel = do
        _ <- takeWhileP Nothing (/= '-')
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

parse :: Text.Text -> IO ()
parse input = do
    output <- parseTest parser $ input
    return output
