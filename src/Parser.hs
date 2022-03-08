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

anyCharP :: Parser Char
anyCharP = P $ \input -> case input of
    (c:cs) -> Just (c, rest)
    []     -> Nothing

charP :: Char -> Parser ()
charP c = do
    c' <- anyCharP
    if c == c'
        then return ()
        else noParserP

anyCharButP :: Char -> Parser Char
anyCharButP c = do
    c' <- anyCharP
    if c /= c'
        then return c'
        else noParserP

letterOrDigitP :: Parser Char
letterOrDigitP = do
    c <- anyCharButP
    if isAlphaNum
        then return c
        else noParserP

orElseP :: Parser a -> Parser a -> Parser a
orElseP p1 p2 = P $ \input -> case runParser p1 input of
    Just r  -> Just r
    Nothing -> runParser p2 input

manyP :: Parser a -> Parser [a]
manyP p = (pure (:) <*> p <*> manyP p) `orElseP` pure []

many1P :: Parser a -> Parser [a]
many1P p = pure (:) <*> p <*> manyP p

sepByP :: Parser a -> Parser () -> Parser [a]
sepByP p1 p2 = ((:) <$> p1 <*> (manyP (p2 *> p1))) `orElseP` pure []
