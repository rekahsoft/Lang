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

-- File: PostFixExpressions.hs
-- Author: Collin J. Doering <rekahsoft@gmail.com>
-- Date: Jul  8, 2012

import Control.Monad
import Data.Monoid
import Text.ParserCombinators.Parsec

data Expression a = Number a
                  | Operator Char (a -> a -> a)

instance Show a => Show (Expression a) where
  show (Number a) = show a
  show (Operator c _) = show c

data Stack a = Stack [Expression a]

instance Show a => Show (Stack a) where
  show (Stack [x]) = show x
  show (Stack (x:xs)) = show x ++ " " ++  show (Stack xs)

-- takes a properly formatted Stack to compute the value of the postfix expression
eval :: (Stack a) -> a
eval (Stack xs) = evaluate xs []

evaluate :: [Expression a] -> [Expression a] -> a
evaluate [] [(Number acc)] = acc
evaluate [] _ = error "Cannot evaluate, leftover operator!"
evaluate [(Number a)] [] = a
evaluate [(Operator _ _)] [] = error "Cannot evaluate, leftover operator!"
evaluate (x@(Number _):xs) acc = evaluate xs (acc ++ [x])
evaluate (x@(Operator c f):xs) ((Number a):(Number b):ys) = evaluate xs $ (Number (f b a)):ys
evaluate ((Operator _ _):xs) _ = error "Cannot evaluate, leftover operator!"

parseNumber :: Parser (Expression Float)
parseNumber = try parseFloat
          <|> parseInt
          
parseInt :: Parser (Expression Float)
parseInt = do x <- many1 digit
              return $ Number (read x :: Float)

parseFloat :: Parser (Expression Float)
parseFloat = do x <- many1 digit
                char '.'
                y <- many1 digit
                return $ Number (read (x ++ "." ++ y) :: Float)

parseOperator :: Parser (Expression Float)
parseOperator = do c <- oneOf "+-*/^"
                   case c of
                     '+' -> return $ Operator '+' (+)
                     '-' -> return $ Operator '-' (-)
                     '*' -> return $ Operator '*' (*)
                     '/' -> return $ Operator '/' (/)
                     '^' -> return $ Operator '^' (**)
                     otherwise -> error $ "Unknown operator \'" ++ [c] ++ "\'"

parsePostfixExpr :: Parser (Stack Float)
parsePostfixExpr = do a <- parseNumber
                      spaces
                      b <- parseNumber
                      spaces
                      xs <- sepBy (parseNumber <|> parseOperator) spaces
                      return $ Stack $ a:b:xs

-- instance Monoid (Expression a) where
--   mempty = Number []
  
--   mappend (Number xs) (Number ys) = Number $ xs ++ ys
--   mappend (Value x) (Number ys) = Number $ x:ys
--   mappend (Number xs) (Value y) = Number $ y:xs
--   mappend (Value x) (Value y) = Number $ [x,y]

-- sepBy2 p sep = p >>= \x -> do xs <- many1 p
--                               return $ x:xs

-- parseNumber :: Parser (Expression Float)
-- parseNumber = do x <- many1 digit
--                  (try $ char '.' >> do y <- many1 digit
--                                        return $ Value (read (x ++ y) :: Float))
--                    <|> (return $ Value (read x :: Float))

-- --parseManyNumbers :: Parser
-- --parseManyNumbers st = liftM mconcat $ sepBy2 parseNumber spaces st

-- --parseOperator :: (Expression a) -> Parser (Expression a)
-- parseOperator st = do op <- oneOf "+-*/"
--                       return $ mappend (Value $ apply op (stk !! 1) (stk !! 0)) $ Number $ drop 2 stk
--   where numberStk (Number xs) = xs
--         numberStk (Value x) = [x]
--         stk = numberStk st
--         apply '*' a b = a * b
--         apply '/' a b = a / b
--         apply '+' a b = a + b
--         apply '-' a b = a - b

-- -- parsePostfixExpr :: Parser (Expression a)
-- -- parsePostfixExpr = do xs <- parseManyNumbers mempty
-- --                       ys <- parseOperator xs
-- --                       many (parseManyNumbers ys <|> parseOperator ys)
  
--   -- many1 (parseManyNumbers
--   --                         >>= parseOperator) <|> eof
                      
