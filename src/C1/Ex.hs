module C1.Ex where

import Data.List (intercalate, (!?), inits, tails)
import Control.Arrow
import Data.Ratio
import Data.Map qualified as Map
import Data.Map (Map)

{-|
 Run-length encoding of a list.



Implement the so-called run-length encoding data compression method.

Consecutive duplicates of elements are encoded as lists (N E) where N is the number of
duplicates of the element E.

>>> encode 'aaaabccaadeeee'
[(4,'a'),(1,'b'),(2,'c'),(2,'a'),(1,'d'),(4,'e')]
-}
encode :: forall a. Eq a => [a] -> [(Int, a)]
encode = foldr go []
    where
    go x [] = [(1, x)]
    go x ((n, y):xs)
        | x == y = (n + 1, y) : xs
        | otherwise = (1, x) : (n, y) : xs

{-|
 Run-length deecoding of a list.

>>> decode (encode 'aaaabccaadeeee')
'aaaabccaadeeee'
-}
decode :: forall a. [(Int, a)] -> [a]
decode = concatMap $ uncurry replicate


{-|
Crypto poetry
https://onlinejudge.org/external/112/11220.pdf

11220 Decoding the message
Chico and Maria are relatives that live in different towns. As they inhabit a rural area, it is very
difficult for them to keep in touch. One way they found to overcome their communication problem was
to send a line through their parents that used to visit each other.
The point is that Chico and Maria did not want that their parents read their messages, and they
decided to create a secret code for the messages. The code is not very sophisticated, but you should
keep in mind Chico and Maria are just children.
In general, the meaning of a message is based on a letter of each word, in a way that they will form a
message with the first letter of the first word, the second letter of the second word and so on. If a word
does not have enough letters, the following word should be used. For example, if you are analyzing the
third word, you should consider its third letter, but if it just has two letters, then you should try to
form a decoded word with the third letter of the fourth word.
When the end of a line is reached, you should finish the current decoded word and should start to
form another one from the first letter of the first word in the next line.
Your task is to translate a message according to Chico and Maria’s secret code.
-}
cryptoPoetry :: String -> String
cryptoPoetry = unwords  . fmap lineT   . lines
    where
    lineT :: String -> String
    lineT = snd . foldl wordT  (1,"") . words

    wordT :: (Int,String) -> String -> (Int,String)
    wordT (n,acc) word
        | n > length word = (n,acc)
        | otherwise = (n + 1, acc <> [word !! (n - 1)] )

cryptoEx :: String
cryptoEx = unlines
    [ "Hey good lawyer"
    , "as I previously previewed"
    , "yam does a soup"
    ]

{-|
 -Jingle Composition
https://matcomgrader.com/problem/9386/jingle-composing/

v2: do all the compasses have the same duration?
-}
jingleCompV2 :: String -> Bool
-- jingleCompV2 = (\(a,b) -> all (== a) b) . (safeFst 0 &&& id) . fmap measureDuration . measures
-- jingleCompV2 = uncurry ($) . first (all . (==)) . (safeFst 0 &&& id) . fmap measureDuration . measures
jingleCompV2 = uncurry ($) . (all . (==) . safeFst 0 &&& id) . fmap measureDuration . measures


    where
    safeFst :: a -> [a] -> a
    safeFst d [] = d
    safeFst _ (x:_) = x

    measures :: String -> [String]
    measures xs
        = let (m,r) = (break (== '/') . drop 1) xs
          in if null r then [] else m : measures r

    noteMap :: [(Char, Rational)]
    noteMap = [ ('W', 1)
              , ('H', 1/2)
              , ('Q', 1/4)
              , ('E', 1/8)
              , ('S', 1/16)
              , ('T', 1/32)
              , ('X', 1/64)
              ]

    noteDuration :: Char -> Rational
    noteDuration c = case lookup c noteMap of
        Just d -> d
        Nothing -> error $ "Invalid note: " ++ [c]

    measureDuration :: String -> Rational
    measureDuration = sum . map noteDuration

{-|
 -https://exercism.org/tracks/haskell/exercises/pythagorean-triplet
 -}
pythagoreanTriplet :: Int -> Maybe (Int, Int, Int)
pythagoreanTriplet c = [ (a,b,c) | b <- [2..c], a <- [1..b-1], a^2 + b^2 == c^2] !? 0



newtype Mean a = Mean { getMean :: (a, Int) }

instance Num a => Semigroup (Mean a) where
    Mean (x0,x1) <> Mean (y0,y1) = Mean (x0 + y0, x1 + y1)

instance Num a => Monoid (Mean a) where
    mempty = Mean (0,1)

normalizeMean :: Fractional a => Mean a -> a
normalizeMean (Mean (x, n)) = x / fromIntegral n

mean :: (Foldable f, Fractional a) => f a -> a
mean =  normalizeMean . foldMap single
    where
    single x = Mean (x, 1)

groupBy' :: (Foldable t, Ord k) => (a -> k) -> t a -> Map k [a]
groupBy' f  = Map.fromListWith (<>) . foldr  (\x xs -> (f &&& pure) x : xs) []


plotHisto :: Foldable f => Int -> f Double -> String
plotHisto buckets xs = boxes
    where
        minX, maxX :: Double
        minX = minimum xs
        maxX = maximum xs
        bucketLength :: Int
        bucketLength = ceiling $ (maxX - minX) / fromIntegral buckets

        aux :: Map Int Int
        aux = fmap length $ flip groupBy' xs $ \x -> floor $ (x - minX) / fromIntegral bucketLength

        maxKey = case Map.lookupMax aux of
            Just m -> m
            _ -> error "empty histo"

        maxKeyLength = length maxKey
        pad s = s <> replicate (maxKeyLength - length s) ' '

        _3f :: String -> String
        _3f = (\(x,y) -> x <> take 4 y) . span (/= '.')

        _3f' :: String -> String
        _3f' = uncurry mappend . second  (take 4) . span (/= '.')


        boxes :: String
        boxes = flip Map.foldMapWithKey aux $ \k v -> pad (_3f $ show $ fromIntegral (k * bucketLength) + minX) <> " | " <> replicate v '▨' <> "\n"

testHisto :: IO ()
testHisto = putStrLn $ plotHisto 5
    [ 50,53,57
    , 60,61,68,68,69
    , 70,70,71,71,72,72,73,73,74,74,75,75
    , 86,87,88,89,89,89,89
    , 91,95,99
    ]
