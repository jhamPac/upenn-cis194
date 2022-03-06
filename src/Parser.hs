module Parser where

newtype Parser a = P (String -> Maybe (a, String))
