-- File: StringParse.hs
-- Date: Oct 26, 2011
-- Author: Collin J. Doering <rekahsoft@gmail.com>
-- Description: This is a test file in haskell implementing a simple base monadic parser

import Monad

-- parser as layed out by Programming in Haskell
newtype Parser a = Parser (String -> [(a, String)])

-- Parser as layed out by RealWorldHaskell
-- import qualified Data.ByteString.Lazy as L
-- data ParseState = ParseState {
--   string :: String
--   offset :: Integer
--  }

-- newtype Parser a = Parser {
--   runParse :: ParseState -> Either String (a, ParseState)
--  }

instance Monad Parser where
  return v = Parser (\inp -> [(v,inp)])
  p >>= q = Parser (\inp -> case parse p inp of
                  [] -> []
                  [(v,out)] -> parse (q v) out)

instance MonadPlus Parser where
  mzero = Parser (\inp -> [])
  mplus p q = Parser (\inp -> case parse p inp of
                    [] -> parse q inp
                    [(v,out)] -> [(v,out)])

failure :: Parser a
failure = Parser (\xs -> [])

item :: Parser Char
item = Parser (\inp -> case inp of
  [] -> []
  (x:xs) -> [(x,xs)])

parseWhile :: (a -> Bool) -> Parser a -> Parser a
parseWhile f (Parser p) = Parser $ \inp -> do
  x <- item
  if f x then parse p else failure

identity :: Parser Char
identity = Parser $ \inp -> []

parse :: Parser a -> String -> [(a,String)]
parse (Parser p) inp = p inp

p :: Parser (Char, Char)
p = do
  x <- item
  item
  y <- item
  return (x,y)

takeOneLeaveOne :: Parser Char
takeOneLeaveOne = do
  x <- item
  item
  return x
  
--takeEveryOther :: Parser String
  