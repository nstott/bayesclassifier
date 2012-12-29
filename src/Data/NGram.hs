module Data.NGram where

    -- TODO should lowercase and apply a stemmer to the ngrams

    import Data.List (intersperse, (\\))
    import Data.Maybe (fromMaybe)
    import qualified Data.Map as Map
    import NLP.Stemmer
    import qualified Data.Char as Char

    type NGram = String
    type NGrams = Map.Map NGram Int

    empty :: NGrams
    empty = Map.empty

    explode :: String -> [String]
    explode x =  words $ foldl1 (++) $ lines x

    implode :: [String] -> String
    implode x = foldl1 (++) $ intersperse " " x

    fromString :: Int -> String -> NGrams
    fromString p s = toNGram p [] $ explode s

    toNGram :: Int -> [String] -> [String] -> NGrams
    toNGram part l stopWords = doit (l \\ stopWords) Map.empty
        where
            doit :: [String] -> NGrams -> NGrams
            doit [] acc = acc
            doit lst@(_:xs) acc = doit xs $ Map.insertWith (+) (cleanWords $ take part lst) 1 acc

    cleanWords :: [String] -> String
    cleanWords lst = implode $ stemWords Porter $ map (map Char.toLower) lst

    toTriGram :: [String] -> [String] -> NGrams
    toTriGram = toNGram 3

    toBiGram :: [String] -> [String] -> NGrams
    toBiGram = toNGram 2

    toOneGram :: [String] -> [String] -> NGrams
    toOneGram = toNGram 1

    getCount :: String -> NGrams -> Int
    getCount s ng = fromMaybe 0 (Map.lookup s ng)

    union :: NGrams -> NGrams -> NGrams
    union = Map.unionWith (+)

    countNGrams :: NGrams -> Int
    countNGrams n = Map.foldl (+) 0 n
