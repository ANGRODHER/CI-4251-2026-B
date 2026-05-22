module C3.Ex where

import Text.Parsec
import Data.Map (Map)
import Data.Map qualified as Map
import Data.Maybe
import Data.List (intercalate)

data JSonValue
    = JString String
    | JNum Double
    | JObject JSon
    | JArray [JSonValue]
    | JBool Bool

instance Show JSonValue where
    show (JString s) = show s
    show (JNum n) = show n
    show (JObject o) = show o
    show (JArray a) = "[" ++ intercalate ", " (map show a) ++ "]"
    show (JBool b) = if b then "true" else "false"



newtype JSon = JSon (Map String JSonValue)

instance Show JSon where
    show (JSon m) = "{" ++ intercalate ", " (map showPair (Map.toList m)) ++ "}"
        where
        showPair (k, v) = show k ++ ": " ++ show v

parseJSon :: Parsec String () JSon
parseJSon = between (char '{') (char '}')
    $ (pure f <* spaces')
    <*> ( (
            (,)
            <$> (pString <* spaces' <* char ':' <* spaces')
            <*> parseJSonValue
          ) `sepBy` (spaces' *> char ',' *> spaces'))
    where
    f :: [(String, JSonValue)] -> JSon
    f = JSon . Map.fromList

parseJSonValue :: Parsec String () JSonValue
parseJSonValue = pWhiteSpace *>
        ( pStringValue
        <|> pNumberValue
        <|> pObjectValue
        <|> pArrayValue
        <|> pBoolValue
        ) <* pWhiteSpace where
    pStringValue = JString <$> pString
    pNumberValue = JNum <$> pNumber
    pObjectValue = JObject <$> parseJSon
    pArrayValue  = JArray <$> between (char '[') (char ']') (parseJSonValue `sepBy` (spaces' *> char ',' *> spaces'))
    pBoolValue   = JBool <$> ((True <$ string "true") <|> (False <$ string "false"))

pWhiteSpace :: Parsec String () ()
--pWhiteSpace = (space <|> newline <|> crlf <|> tab) *> optional pWhiteSpace
pWhiteSpace = skipMany $ space <|> newline <|> crlf <|> tab

spaces' :: Parsec String () ()
spaces' = pWhiteSpace



pNumber :: Parsec String () Double
pNumber = f
    <$> optionMaybe (char '-')
    <*> many1 digit
    <*> optionMaybe (char '.' *> many1 digit)
    <*> optionMaybe
        ( (char 'e' <|> char 'E')
        *> pure (,)
        <*> optionMaybe (char '+' <|> char '-')
        <*> many1 digit
    )
    where
    f :: Maybe Char -> [Char] -> Maybe [Char] -> Maybe (Maybe Char, [Char]) -> Double
    f mSign intPart mFrac mExp =
        let sign = fromMaybe ' ' mSign
            frac = maybe "" ('.' :) mFrac
            exp  = case mExp of
                Nothing -> ""
                Just (mSign', expPart) -> "e" <> (fromMaybe ' ' mSign' : expPart)
        in read $ (sign : intPart) <> frac  <> exp

pString :: Parsec String () String
pString = between (char '"') (char '"') $ pString' where
    pString' :: Parsec String () String
    pString' = fmap concat . many $ ((:[]) <$> noneOf ['"','\\']) <|>
        (f
        <$> char '\\'
        <*> ( (pure <$> (choice . fmap char) ['"', '\\', '/', 'b', 'f', 'n', 'r', 't'])
            <|> ( (:) <$> char 'u' <*> count 4 hexDigit )
            )
        )
    f :: Char -> String -> String
    f = (:)
