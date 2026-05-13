module C3.Ex1 where

import Text.Parsec
import Data.Map (Map)
import Data.Map qualified as Map
import Data.Maybe
import Data.List (intercalate)
import Data.Functor


data JSonValue
    = JString String
    | JNum   Double
    | JObject JSon
    | JArray [JSonValue]
    | JBool Bool
    | JNull
instance Show JSonValue where
    show (JString s) = show s
    show (JNum n) = show n
    show (JObject o) = show o
    show (JArray a) = "[" ++ intercalate ", " (map show a) ++ "]"
    show (JBool b) = if b then "true" else "false"
    show JNull = "null"


newtype JSon = JSon (Map String JSonValue)

instance Show JSon where
    show (JSon m) = "{" ++ intercalate ", " (map showPair (Map.toList m)) ++ "}"
        where
        showPair (k, v) = show k ++ ": " ++ show v

pJSon :: Parsec String () JSon
pJSon = between (char '{') (char '}')
    $ f <$> (( (,)
        <$> (pWhiteSpace *> pString <* pWhiteSpace <* char ':')
        <*> pJSonValue
    ) `sepBy` char ',')
    where
        f :: [(String,JSonValue)] -> JSon
        f = JSon . Map.fromList

pJSonValue :: Parsec String () JSonValue
pJSonValue = pWhiteSpace *>
    ( pStringValue
    <|> pNumberValue
    <|> pObjectValue
    <|> pArrayValue
    <|> pBoolValue
    <|> pNullValue
    ) <* pWhiteSpace
    where
    pStringValue, pNumberValue, pObjectValue, pArrayValue, pBoolValue, pNullValue :: Parsec String () JSonValue

    pStringValue = JString <$> pString
    pNumberValue = JNum    <$> pNumber
    pObjectValue = JObject <$> pJSon
    pArrayValue  = JArray . fmap JObject <$>
        between (char '[') (char ']')  (pJSon `sepBy` (char ',' *> spaces'))
    pBoolValue   = JBool   <$> ( (string "true" $> True) <|> (string "false" $> False))
    pNullValue   = JNull <$ string "null"

-- pure x <* f a = x <$ f a = fmap (const x) (f a)



-- Alternative ~ Monoid for Applicatives
pWhiteSpace :: Parsec String () ()
-- pWhiteSpace =
--     (
--         (char ' ' <|> char '\r' <|> char '\n' <|> char '\t') *> pWhiteSpace
--     ) <|> pure ()
pWhiteSpace = skipMany $ (choice . fmap char) " \r\n\t"

spaces' :: Parsec String () ()
spaces' = pWhiteSpace


-- f <$> fa = pure f <*> fa



pNumber :: Parsec String () Double
pNumber = f <$> pSign <*> pDigits <*> pFrac <*> pExp
    where
    pSign :: Parsec String () (Maybe Char)
    pSign = optionMaybe $ char '-'

    pDigits :: Parsec String () String
    pDigits = many1 digit

    pFrac :: Parsec String () (Maybe String)
    pFrac = optionMaybe $  (:) <$> char '.' <*> many1 digit

    pExp :: Parsec String () (Maybe String)
    pExp
        = optionMaybe
        $ g
        <$> (char 'e' <|> char 'E')
        <*> optionMaybe (char '-' <|> char '+')
        <*> many1 digit

    g :: Char -> Maybe Char -> String -> String
    g e mSign digits
        = (e : maybe "" (:[]) mSign) <> digits

    f :: Maybe Char -> String -> Maybe String -> Maybe String -> Double
    f mSign intPart mFrac mE
        = read
        $  maybe "" pure mSign
        <> intPart
        <> fromMaybe "" mFrac
        <> fromMaybe "" mE


-- (*>) :: f a -> f b -> f b
-- a *> b = pure const <*> a <*> b
--
-- (<*) :: f a -> f b -> f a
-- a <* b = pure (flip const) <*> a <*> b

--pure :: a -> [a]

pString :: Parsec String () String
pString = between (char '"') (char '"') middle where
    middle :: Parsec String () String
    middle = fmap concat . many $ (pure <$> noneOf ['"','\\']) <|>
        ( (:)
        <$> char '\\'
        <*> ( pure <$> (choice . fmap char) ['"','\\','/','b','f','r','n','t']
            <|> ( (:) <$> char 'u' <*> count 4 hexDigit)
            )
        )
