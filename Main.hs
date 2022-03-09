module Main where

import Data.Char
import Control.Applicative

data JsonValue
    = JsonNull
    | JsonBool Bool
    | JsonNumber Integer -- No support for floats
    | JsonString String
    | JsonArray [JsonValue]
    | JsonObject [(String, JsonValue)]
    deriving (Show, Eq)

-- NOTE: no proper error reporting
newtype Parser a = Parser
    { runParser :: String -> Maybe (String, a)
    }

instance Functor Parser where
    fmap f (Parser p) = Parser $ \input -> do
                            (input', x) <- p input
                            Just (input', f x)

instance Applicative Parser where
    pure x = Parser $ \input -> Just (input, x)
    (Parser p1) <*> (Parser p2) = Parser $ \input -> do
                                    (input', f) <- p1 input
                                    (input'', a) <- p2 input'
                                    Just (input'', f a)

instance Alternative Parser where
    empty = Parser $ \_ -> Nothing
    (Parser p1) <|> (Parser p2) = 
        Parser $ \input -> p1 input <|> p2 input

charParser :: Char -> Parser Char
charParser x = Parser f
                where 
                    f(y:ys)
                        | y == x = Just (ys, x)
                        | otherwise = Nothing
                    f [] = Nothing

stringParser :: String -> Parser String
stringParser = sequenceA . map charParser

jsonNull :: Parser JsonValue
jsonNull = (\_ -> JsonNull) <$> stringParser "null"

jsonBool :: Parser JsonValue
jsonBool = f <$> (stringParser "true" <|> stringParser "false")
    where   f "true" = JsonBool True
            f "false" = JsonBool False
            f _ = undefined

spanParser :: (Char -> Bool) -> Parser String
spanParser f = Parser $ \input -> let (token, rest) = span f input
                                    in Just (rest, token)

jsonNumber :: Parser JsonValue
jsonNumber = f <$> notNull (spanParser isDigit)
    where f ds = JsonNumber $ read ds

stringLiteral :: Parser String
stringLiteral = charParser '"' *> spanParser (/= '"') <* charParser '"'

jsonString :: Parser JsonValue
jsonString = JsonString <$> stringLiteral

ws :: Parser String
ws = spanParser isSpace

sepBy :: Parser a -> Parser b -> Parser [b]
sepBy sep element = (:) <$> element <*> many (sep *> element) <|> pure []

notNull :: Parser [a] -> Parser [a]
notNull (Parser p) =
    Parser $ \input -> do
        (input', xs) <- p input
        if null xs
            then Nothing
            else Just (input', xs)

jsonArray :: Parser JsonValue
jsonArray = JsonArray <$> (charParser '[' *> ws *> elements <* ws <* charParser ']')
    where   elements = sepBy sep jsonValue
            sep = ws *> charParser ',' <* ws

jsonObject :: Parser JsonValue
jsonObject = JsonObject <$> (charParser '{' *> ws *> keypairs <* ws <* charParser '}')
    where   keypairs = sepBy (ws *> charParser ',' <* ws) pair
            pair = (\key _ value -> (key, value)) <$> stringLiteral <*> (ws *> charParser ':' <* ws) <*> jsonValue


jsonValue :: Parser JsonValue
jsonValue = jsonNull <|> jsonBool <|> jsonNumber <|> jsonString <|> jsonArray <|> jsonObject

parseFile :: FilePath -> Parser a -> IO (Maybe a)
parseFile fileName parser = do
    input <- readFile fileName
    return (snd <$> runParser parser input)

main :: IO()
main = undefined
