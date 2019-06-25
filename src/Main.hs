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
import qualified Data.Text as Text

import Parser

main :: IO ()
main = do
    input <- IO.readFile "tests/test.skef"
    parse $ Text.pack input
