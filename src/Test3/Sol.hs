{-# LANGUAGE TemplateHaskell #-}

module Test3.Sol where

import Control.Lens hiding (noneOf)
import Data.List
import Data.Maybe
import qualified Data.Map as Map
import Text.Parsec
import Data.Functor (($>))
import System.IO.Unsafe (unsafePerformIO)


-- 1. TIPOS DE DATOS Y LENTES (DEBEN IR AL PRINCIPIO)

data Evolution = Evolution
    { _eNum  :: String
    , _eName :: String
    }
    deriving (Eq,Show)

makeLenses ''Evolution

data Pokemon = Pokemon
    { _pokeId        :: Int
    , _num           :: String
    , _name          :: String
    , _img           :: String

    , _types         :: [String]

    , _height        :: Double
    , _weight        :: Double

    , _candy         :: String
    , _candyCount    :: Maybe Int

    , _egg           :: Maybe String

    , _spawnChance   :: Double
    , _avgSpawns     :: Double
    , _spawnTime     :: String

    , _multipliers   :: Maybe [Double]

    , _weaknesses    :: [String]

    , _nextEvolution :: [Evolution]
    , _prevEvolution :: [Evolution]
    }
    deriving (Eq,Show)

makeLenses ''Pokemon

newtype PokeSet =
    PokeSet
        { _pokemons :: [Pokemon]
        }
    deriving (Eq,Show)

makeLenses ''PokeSet
pokeSet :: PokeSet
pokeSet = either error id (unsafePerformIO (pPokeSet <$> readFile "pokedex.json"))

-- 2. PARSER JSON Y LÓGICA 
data JSonValue
    = JString String
    | JNum   Double
    | JObject JSon
    | JArray [JSonValue]
    | JBool Bool
    | JNull

newtype JSon = JSon (Map.Map String JSonValue)

-- Definición de espacios en blanco 
pWhiteSpace :: Parsec String () ()
pWhiteSpace = spaces

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
    pArrayValue  = JArray <$>
        between (char '[') (char ']')  (pJSonValue `sepBy` (char ',' *> pWhiteSpace))
    pBoolValue   = JBool   <$> ( (string "true" $> True) <|> (string "false" $> False))
    pNullValue   = JNull <$ string "null"

-- pure x <* f a = x <$ f a = fmap (const x) (f a)

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

lookupField :: String -> JSon -> Either String JSonValue
lookupField k (JSon m) =
    maybe
        (Left ("Missing field: " ++ k))
        Right
        (Map.lookup k m)

asString :: JSonValue -> Either String String
asString (JString s) = Right s
asString _           = Left "Expected string"


asDouble :: JSonValue -> Either String Double
asDouble (JNum n) = Right n
asDouble _        = Left "Expected number"

asInt :: JSonValue -> Either String Int
asInt (JNum n) = Right (round n)
asInt _        = Left "Expected int"

asArray :: JSonValue -> Either String [JSonValue]
asArray (JArray xs) = Right xs
asArray _           = Left "Expected array"

asObject :: JSonValue -> Either String JSon
asObject (JObject o) = Right o
asObject _           = Left "Expected object"

parseEvolution :: JSon -> Either String Evolution
parseEvolution j = do
    num'  <- lookupField "num" j >>= asString
    name' <- lookupField "name" j >>= asString

    pure $
        Evolution
            num'
            name'

parseMultipliers :: JSonValue -> Either String (Maybe [Double])
parseMultipliers JNull =
    Right Nothing

parseMultipliers (JArray xs) =
    Just <$> mapM asDouble xs

parseMultipliers _ =
    Left "Invalid multipliers"


parseEvolutionList :: Maybe JSonValue -> Either String [Evolution]
parseEvolutionList Nothing =
    Right []

parseEvolutionList (Just (JArray xs)) =
    mapM
        (\v -> do
            o <- asObject v
            parseEvolution o
        )
        xs

parseEvolutionList _ =
    Left "Invalid evolution list"  

parsePokemon :: JSon -> Either String Pokemon
parsePokemon j = do

    pid <- lookupField "id" j >>= asInt

    num' <- lookupField "num" j >>= asString

    name' <- lookupField "name" j >>= asString

    img' <- lookupField "img" j >>= asString

    types' <- lookupField "type" j >>= asArray >>= mapM asString

    heightStr <- lookupField "height" j >>= asString

    weightStr <- lookupField "weight" j >>= asString

    candy' <- lookupField "candy" j >>= asString

    let candyCountVal =
            case Map.lookup "candy_count" m of
                Just (JNum n) -> Just (round n)
                _             -> Nothing

    egg' <- lookupField "egg" j >>= parseEgg

    spawnChance' <- lookupField "spawn_chance" j >>= asDouble

    avgSpawns' <- lookupField "avg_spawns" j >>= asDouble

    spawnTime' <- lookupField "spawn_time" j >>= asString

    multipliers' <-
        lookupField "multipliers" j
            >>= parseMultipliers

    weaknesses' <-
        lookupField "weaknesses" j
            >>= asArray
            >>= mapM asString

    let nextVal = Map.lookup "next_evolution" m
    let prevVal = Map.lookup "prev_evolution" m

    nextE <- parseEvolutionList nextVal
    prevE <- parseEvolutionList prevVal

    pure Pokemon
        { _pokeId = pid
        , _num = num'
        , _name = name'
        , _img = img'
        , _types = types'
        , _height = read (takeWhile (/=' ') heightStr)
        , _weight = read (takeWhile (/=' ') weightStr)
        , _candy = candy'
        , _candyCount = candyCountVal
        , _egg = egg'
        , _spawnChance = spawnChance'
        , _avgSpawns = avgSpawns'
        , _spawnTime = spawnTime'
        , _multipliers = multipliers'
        , _weaknesses = weaknesses'
        , _nextEvolution = nextE
        , _prevEvolution = prevE
        }
  where
    JSon m = j

parseEgg :: JSonValue -> Either String (Maybe String)

parseEgg JNull =
    Right Nothing

parseEgg (JString s) =
    Right (Just s)

parseEgg _ =
    Left "Invalid egg"
                 





pPokeSet :: String -> Either String PokeSet
pPokeSet txt =
    case parse pJSon "" txt of
        Left err ->
            Left (show err)

        Right root@(JSon m) ->
            case Map.lookup "pokemon" m of

                Just (JArray xs) ->
                    PokeSet <$> mapM parsePoke xs

                _ ->
                    Left "Missing pokemon array"
  where
    parsePoke v = do
        o <- asObject v
        parsePokemon o

pokeNames :: [String]
pokeNames =
    pokeSet ^.. pokemons . traversed . name

pokeEvolutions :: [(String,[String])]
pokeEvolutions =
    pokeSet ^..
        pokemons .
        traversed .
        to
            (\p ->
                ( p ^. name
                , p ^.. nextEvolution . traversed . eName
                )
            )

pokeEvolutions' :: [(String,[String])]
pokeEvolutions' =
    pokeSet ^..
        pokemons .
        traversed .
        filtered
            (\p ->
                null (p ^. prevEvolution)
            ) .
        to
            (\p ->
                ( p ^. name
                , p ^.. nextEvolution . traversed . eName
                )
            )

pokePsychicNormal :: PokeSet
pokePsychicNormal =
    pokeSet &
        pokemons .
        traversed .
        filtered
            (\p ->
                "Psychic" `elem` (p ^. types)
                &&
                "Normal" `elem` (p ^. types)
            ) .
        multipliers . traversed . traversed
        +~ 2

pokePsychicNormal' :: PokeSet
pokePsychicNormal' =
    pokeSet &
        pokemons .
        traversed .
        filtered
            (\p ->
                "Psychic" `elem` (p ^. types)
                ||
                "Normal" `elem` (p ^. types)
            ) .
        multipliers . traversed . traversed
        -~ 1

addTuff :: String -> String
addTuff s
    | "tuff" `isSuffixOf` s = s
    | otherwise             = s ++ " tuff"

pokeTuff :: PokeSet
pokeTuff = pokeSet
    & pokemons . traversed . name %~ addTuff
    & pokemons . traversed . nextEvolution . traversed . eName %~ addTuff
    & pokemons . traversed . prevEvolution . traversed . eName %~ addTuff

newtype Histo = Histo String
instance Show Histo where
    show (Histo s) = s

pokeWeakest :: [String]
pokeWeakest =
    pokeSet ^..
        pokemons .
        traversed .
        filtered
            (\p ->
                length (p ^. weaknesses)
                ==
                maximum
                    ( pokeSet ^..
                        pokemons .
                        traversed .
                        to (length . view weaknesses)
                    )
            ) .
        name

pokeAvgWeight :: Double
pokeAvgWeight =
    sumOf
        ( pokemons .
          traversed .
          weight
        )
        pokeSet
    /
    fromIntegral
        ( length
            ( pokeSet ^..
                pokemons .
                traversed
            )
        )

pokeVarWeight :: Double
pokeVarWeight =
    sum
        ( pokeSet ^..
            pokemons .
            traversed .
            weight .
            to
                (\w ->
                    (w - pokeAvgWeight) * (w - pokeAvgWeight)
                )
        )
    /
    fromIntegral
        ( length
            ( pokeSet ^..
                pokemons .
                traversed
            )
        )

pokeDrinker :: PokeSet
pokeDrinker = pokeSet & pokemons . traversed %~ \p ->
    p & img %~ \currentImg ->
        fromMaybe currentImg (pokeSet ^? pokemons . traversed . filtered (\y -> Just (y ^. num) == (p ^? nextEvolution . _head . eNum) && y ^. weight > p ^. weight) . img)

pokeCorr :: Double
pokeCorr =
    ( sumOf (pokemons . traversed . to (\p -> (p ^. weight - pokeAvgWeight) * (p ^. height - (sumOf (pokemons . traversed . height) pokeSet / fromIntegral (length (pokeSet ^.. pokemons . traversed)))))) pokeSet )
    /
    sqrt ( pokeVarWeight * fromIntegral (length (pokeSet ^.. pokemons . traversed)) * sumOf (pokemons . traversed . to (\p -> (p ^. height - (sumOf (pokemons . traversed . height) pokeSet / fromIntegral (length (pokeSet ^.. pokemons . traversed))))^2)) pokeSet )

-- Función auxiliar
spawnTimeToSeconds :: String -> Double
spawnTimeToSeconds "N/A" = 0
spawnTimeToSeconds s = 
    case break (== ':') s of
        (m, _:s') -> read m * 60 + read s'
        _         -> 0

pokeIQR :: (Double, Double, Double, Double)
pokeIQR =
    (\xs ->
        ( xs !! (length xs `div` 4)
        , xs !! (length xs `div` 2)
        , xs !! (3 * length xs `div` 4)
        , xs !! (3 * length xs `div` 4)
            -
          xs !! (length xs `div` 4)
        )
    )
    ( sort
        ( pokeSet ^..
            pokemons .
            traversed .
            spawnTime .
            to spawnTimeToSeconds
        )
    )

type ContingencyTable = Map.Map (String, String) Int

pokeContingency :: ContingencyTable
pokeContingency = 
    foldl' (\acc (t, w) -> Map.insertWith (+) (t, w) 1 acc) Map.empty
    ( pokeSet ^.. pokemons . traversed . to (\p -> [(t, w) | t <- p ^. types, w <- p ^. weaknesses]) . traversed )


pokeBoxPlot :: String
pokeBoxPlot =
    (\(q1,q2,q3,iqr) ->
        unlines
            [ "Q1  = " ++ show q1
            , "Q2  = " ++ show q2
            , "Q3  = " ++ show q3
            , "IQR = " ++ show iqr
            ]
    )
    pokeIQR

pokeHist :: Histo
pokeHist = Histo
    ( unlines
    $ map (\(k, v) -> justify k ++ ": " ++ replicate v '*')
    $ Map.toList
    $ Map.fromListWith (+)
    $ map (\x -> (x, 1))
    $ (pokeSet ^.. pokemons . traversed . egg . to (fromMaybe "Not in Eggs"))
    )
  where
    justify s = s ++ replicate (12 - length s) ' '



{-
Decisión de Diseño:
se ha modelado el dataset utilizando tipos de datos algebraicos estructurados mediante registros jerárquicos (PokeSet, Pokemon, Evolution), abstrayendo los valores primitivos del JSON a tipos fuertes de Haskell (Int, Double, String, Maybe).
Esto permite seguridad de tipos que garantiza en tiempo de compilación que operaciones matemáticas solo se ejecuten sobre campos genuinamente numéricos, previniendo errores de lógica.
ademas la compatibilidad automática con opticas al utilizar la sintaxis de registros, la directiva makeLenses de Control.Lens genera limpiamente lentes puras para cada campo, permitiendo lecturas y modificaciones complejas en estructuras anidadas.

la ausencia de datos mediante el tipo Maybe (candyCount, multipliers) fuerza al desarrollador a lidiar explícitamente con los casos nulos de forma segura.


En cuanto a las opticas:
la elección de cada combinador se basó estrictamente en la naturaleza de la consulta (Lectura destructiva vs Modificación de Estructuras):

Se seleccionaron combinadores de tipo Fold para consultas orientadas a la extracción y reducción de datos 
(como pokeNames, pokeAvgWeight, pokeVarWeight, pokeCorr). Dado que estas funciones requerían consolidar múltiples valores en una lista 
o en un escalar unificado sin alterar el estado original del dataset, un Fold representa la abstracción de solo-lectura lo que es ideal que preserva la inmutabilidad de manera eficiente.


Se utilizaron combinadores de tipo Traversal en consultas que exigían transformaciones o actualizaciones de datos en lote manteniendo la estructura intacta 
(como pokePsychicNormal, pokeTuff, pokeDrinker). Un Traversal permite tanto leer como escribir en múltiples focos simultáneamente.
Al combinarse con operadores de modificación abstracta, nos permite alterar propiedades profundas (como multiplicar los factores de daño o concatenar sufijos a strings) 
de forma pura y en una sola línea de flujo continuo.


Se empleó el combinador to para inyectar funciones de proyección puras (como transformaciones estadísticas personalizadas o mapeos condicionales)
dentro de la cadena de ópticas, manteniendo el diseño libre de variables intermedias locales (let o where).
-}