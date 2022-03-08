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

instance Functor Parser where
    fmap f p = P p'
        where
            p' input = case runParser p input of
                Just (result, rest) -> Just (f result, rest)
                Nothing             -> Nothing

instance Applicative Parser where
    pure = pureParserP
    p1 <*> p2 = P $ \input -> do
        (f, r) <- runParser p1 input
        (x, r') <- runParser p2 r
        return (f x, r')

instance Monad Parser where
    return = pure
    p1 >>= k = P $ \input -> do
        (x, r1) <- runParser p1 input
        runParser (k x) r1
