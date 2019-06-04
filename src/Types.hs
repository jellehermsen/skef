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

module Types where

import qualified Data.Text as Text

data Node = Project Text.Text [Node] 
    | Code Text.Text
    | Todo Text.Text [Node]
    | Done Text.Text [Node]
    | Time Int Text.Text [Node]
    | TextNode Text.Text
    | Custom Text.Text Text.Text [Node]
  deriving (Show)
