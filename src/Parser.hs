module Parser where

import           Data.Char
import           Data.List
import           Data.Maybe
import           System.Environemnt
import           System.Exit
import           System.IO

newtype Parser a = P (String -> Maybe (a, String))

runParser :: Parser a -> String -> Maybe (a, String)
runParser (P p) = p

parse :: Parser a -> String -> Maybe a
parse p input = case runParser p input of
    Just (result, "") -> Just result
    _                 -> Nothing

noParserP :: Parser a
noParserP = P (\_ -> Nothing)

pureParserP :: a -> Parser a
pureParserP x = P (\input -> Just (x, input))
