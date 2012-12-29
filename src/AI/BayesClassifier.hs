module AI.BayesClassifier where

    import qualified Data.NGram as NGram
    import qualified Data.Map as Map


    -- P(class|ngram) = (p(ngram|class) * p(class)) / p(ngram)
    -- p(class|ngram1, ngram2) = (p(ngram1|class) * p(ngram2|class)) / p(ngram)
    -- use ln's and sum them instead

    type ClassificationName = String
    type Document = String
    data Classification = Classification {
        nGrams :: NGram.NGrams,
        numDocuments :: Int,
        numNGrams :: Int} deriving (Show)
    data Corpus = Corpus {
        classifications :: Map.Map String Classification,
        totalDocuments :: Int,
        liklihood :: (Int -> Int -> Int -> Float),
        numAtoms :: Int,
        stopWords :: [String]}

    newClassification :: Classification
    newClassification = Classification NGram.empty 0 0

    newCorpus :: Int -> [String]-> Corpus
    newCorpus = Corpus Map.empty 0 laplacianLiklihood

    findClassification :: ClassificationName -> Corpus -> Classification
    findClassification name corp = Map.findWithDefault newClassification name $ classifications corp

    train :: Document -> ClassificationName -> Corpus -> Corpus
    train
        d name corp@(Corpus {
            classifications = classes,
            totalDocuments = total,
            liklihood = liklihood,
            numAtoms = num}) =
                Corpus {classifications = Map.insert name (addDocument) classes,
                    totalDocuments = total + 1,
                    liklihood = liklihood,
                    numAtoms = num}
                where
                    cls         = findClassification name corp
                    newngrams   = NGram.fromString num [] d
                    newTotal    = numNGrams cls + NGram.countNGrams newngrams
                    addDocument = Classification (NGram.union (nGrams cls) (newngrams)) ((numDocuments cls) + 1) newTotal

    classificationNames :: Corpus -> [ClassificationName]
    classificationNames corp = Map.keys $ classifications corp

    probOfNGram :: NGram.NGram -> ClassificationName -> Corpus -> Float
    probOfNGram n name corp@(Corpus{liklihood = liklihood}) = liklihood num total $ Map.size (nGrams cls)
      where
        cls     = findClassification name corp
        num     = Map.findWithDefault 0 n $ nGrams cls
        total   = numNGrams cls

    getClassificationScores :: Document -> Corpus -> [(ClassificationName, Float)]
    getClassificationScores doc corp = map mapper $ classificationNames corp
        where mapper k = (k, probOfDocumentGivenClassification doc corp k)

    probOfDocumentGivenClassification :: Document -> Corpus -> ClassificationName -> Float
    probOfDocumentGivenClassification
        doc
        corp@(Corpus
            {liklihood = liklihood,
            totalDocuments = total,
            numAtoms = num,
            stopWords = stopWords})
        name = probOfClassification + Map.fold (+) 0 nGramProbabilities
          where
            ngrams                  = NGram.fromString num stopWords doc
            nGramProbabilities      = Map.mapWithKey (\k v -> (fromIntegral v) * log (probOfNGram k name corp)) ngrams
            probOfClassification    = liklihood (numDocuments $ findClassification name corp) total (length $ classificationNames corp)

    getBestClassification :: Document -> Corpus -> (ClassificationName, Float)
    getBestClassification doc corp = foldl bestResult ("No Match Found", -999999999.9) $ getClassificationScores doc corp
      where
        bestResult a b = if (snd a) > (snd b)
            then a
            else b

    -- different types of liklihood
    maxLiklihood :: Int -> Int -> Int -> Float
    maxLiklihood x y _ = (fromIntegral x) / (fromIntegral y)

    laplacianLiklihood :: Int -> Int -> Int -> Float
    laplacianLiklihood x y k = (fromIntegral $ x + 1) / (fromIntegral $ y + k )




