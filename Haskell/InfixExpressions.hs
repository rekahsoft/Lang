-- (C) Copyright Collin Doering 2012
--
-- This program is free software: you can redistribute it and/or modify
-- it under the terms of the GNU General Public License as published by
-- the Free Software Foundation, either version 3 of the License, or
-- (at your option) any later version.
-- 
-- This program is distributed in the hope that it will be useful,
-- but WITHOUT ANY WARRANTY; without even the implied warranty of
-- MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
-- GNU General Public License for more details.
-- 
-- You should have received a copy of the GNU General Public License
-- along with this program.  If not, see <http://www.gnu.org/licenses/>.

-- File: InfixExpressions.hs
-- Author: Collin J. Doering <rekahsoft@gmail.com>
-- Date: Jul  7, 2012

module Main where

import Control.Monad
import Text.ParserCombinators.Parsec
import Data.Char (digitToInt)

data Expression = Expr Char Expression Expression
                | Number Integer

instance Show Expression where
  show expr = show $ evalInfixExpr expr
                               
evalInfixExpr :: Expression -> Integer
evalInfixExpr (Number a) = a
evalInfixExpr (Expr op a b) = case op of
  '+' -> (evalInfixExpr a) + (evalInfixExpr b)
  '-' -> (evalInfixExpr a) - (evalInfixExpr b)
  '*' -> (evalInfixExpr a) * (evalInfixExpr b)
--  '/' -> (evalInfixExpr a) / (evalInfixExpr b)

parseInfixExpr :: Parser Expression
parseInfixExpr = do x <- (parseBracketedInfixExpr <|> parseNumber
                          <?> "number or expression")
                    spaces
                    c <- oneOf "+-*")
                    y <- (parseBracketedInfixExpr <|> parseNumber
                          <?> "number or expression")
                    (try newline >> (return $ Expr c x y))
                      <|> liftM (Expr c x) (parseBracketedInfixExpr <|> parseNumber)
                    
parseNumber :: Parser Expression
parseNumber = do spaces
                 x <- many1 digit
                 return $ Number (read x :: Integer)

parseOperator :: Parse Expression
parseOperator = do spaces
                   c <- oneOf "+-*"
                   case c of
                     '*' -> liftM (Expr '*' 

parseBracketedInfixExpr :: Parser Expression
parseBracketedInfixExpr = do try spaces
                             char '('
                             try spaces
                             x <- try parseInfixExpr <|> parseNumber
                             try spaces
                             char ')'
                             return x

main :: IO ()
main = do ln <- getLine
          case parse parseInfixExpr "infix expr" ln of
            Left err -> error "Parser Error!"
            Right val -> putStrLn $ show val