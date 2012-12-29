module AI.BayesClassifier where

    import qualified Data.NGram as NGram
    import qualified Data.Map as Map


    -- P(class|ngram) = (p(ngram|class) * p(class)) / p(ngram)
    -- p(class|ngram1, ngram2) = (p(ngram1|class) * p(ngram2|class)) / p(ngram)
    -- use ln's and sum them instead

    type ClassificationName = String
    data Classification = Classification { nGrams :: NGram.NGrams, numDocuments :: Int, numNGrams :: Int} deriving (Show)
    data Corpus = Corpus {classifications :: Map.Map String Classification,  totalDocuments :: Int, liklihood :: (Int -> Int -> Int -> Float), numAtoms :: Int}
    type Document = String

    newClassification :: Classification
    newClassification = Classification NGram.empty 0 0

    newCorpus :: Int -> Corpus
    newCorpus = Corpus Map.empty 0 laplacianLiklihood

    addDocument :: Int -> Document -> Classification -> Classification
    addDocument i t (Classification{ nGrams = ngrams, numDocuments = numdocs, numNGrams = numngrams})
        = Classification (NGram.union ngrams newngrams) (numdocs + 1) (numngrams + NGram.countNGrams newngrams)
            where
                newngrams :: NGram.NGrams
                newngrams = NGram.fromString i t

    findClassification :: ClassificationName -> Corpus -> Classification
    findClassification name corp = Map.findWithDefault newClassification name $ classifications corp

    train :: Document -> ClassificationName -> Corpus -> Corpus
    train d name corp@(Corpus classes total liklihood num)
        = Corpus (Map.insert name (addDocument num d $ findClassification name corp) classes) (total + 1) liklihood num

    classificationNames :: Corpus -> [ClassificationName]
    classificationNames corp = Map.keys $ classifications corp

    probOfNGram :: NGram.NGram -> ClassificationName -> Corpus -> Float
    probOfNGram n name corp@(Corpus{liklihood = liklihood})
        = liklihood num total (length $ classificationNames corp)
            where
                cls = findClassification name corp
                num = Map.findWithDefault 0 n $ nGrams cls
                total = numNGrams cls

    probOfDocumentGivenClassification :: Document -> Corpus -> ClassificationName -> Float
    probOfDocumentGivenClassification
        doc
        corp@(Corpus {liklihood = liklihood, totalDocuments = total, numAtoms = num})
        name
            = probOfClassification + Map.fold (+) 0 nGramProbabilities
                where
                    ngrams = NGram.fromString num doc
                    nGramProbabilities = Map.mapWithKey (\k v -> (fromIntegral v) * log (probOfNGram k name corp)) ngrams
                    probOfClassification = liklihood (numDocuments $ findClassification name corp) total (length $ classificationNames corp)

    getClassificationScores :: Document -> Corpus -> [(ClassificationName, Float)]
    getClassificationScores doc corp = map mapper $ classificationNames corp
        where
            mapper k = (k, probOfDocumentGivenClassification doc corp k)

    getBestClassification :: Document -> Corpus -> (ClassificationName, Float)
    getBestClassification doc corp = doit ("No Match Found", -999999999.9) $ getClassificationScores doc corp
        where
            doit :: (ClassificationName, Float) -> [(ClassificationName, Float)] -> (ClassificationName, Float)
            doit m [] = m
            doit m (x:xs)
                | (snd x) > (snd m)     = doit x xs
                | otherwise             = doit m xs

    -- different types of liklihood
    maxLiklihood :: Int -> Int -> Int -> Float
    maxLiklihood x y _ = (fromIntegral x) / (fromIntegral y)

    laplacianLiklihood :: Int -> Int -> Int -> Float
    laplacianLiklihood x y k = (fromIntegral $ x + 1) / (fromIntegral $ y + k )




