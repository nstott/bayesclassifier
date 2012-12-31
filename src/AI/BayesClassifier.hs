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
        classifications :: Map.Map ClassificationName Classification,
        totalDocuments :: Int,
        liklihood :: (Int -> Int -> Int -> Float),
        numAtoms :: Int,
        stopWords :: [String]}

    newClassification :: Classification
    newClassification = Classification NGram.empty 0 0

    newCorpus :: Int -> [String]-> Corpus
    newCorpus = Corpus Map.empty 0 laplacianLiklihood

    findClassification :: ClassificationName -> Corpus -> Classification
    findClassification name corp =
        Map.findWithDefault newClassification name classes
          where
            classes = classifications corp

    train :: Document -> ClassificationName -> Corpus -> Corpus
    train d name corp =
        corp
            { classifications = Map.insert name addDocument classes
            , totalDocuments = 1 + totalDocuments corp}
          where
            cls         = findClassification name corp
            classes     = classifications corp
            newngrams   = NGram.fromString (numAtoms corp) [] d
            newTotal    = numNGrams cls + NGram.countNGrams newngrams
            addDocument = Classification
                { nGrams        = NGram.union (nGrams cls) newngrams
                , numDocuments  = (numDocuments cls) + 1
                , numNGrams     = newTotal}

    classificationNames :: Corpus -> [ClassificationName]
    classificationNames corp = Map.keys $ classifications corp

    probOfNGram :: NGram.NGram -> ClassificationName -> Corpus -> Float
    probOfNGram n name corp = fn num total $ Map.size (nGrams cls)
      where
        cls     = findClassification name corp
        num     = Map.findWithDefault 0 n $ nGrams cls
        total   = numNGrams cls
        fn      = liklihood corp

    getClassificationScores ::  Document ->
                                Corpus ->
                                [(ClassificationName, Float)]
    getClassificationScores doc corp = map mapper $ classificationNames corp
        where mapper k = (k, probOfDocumentGivenClass doc corp k)

    probOfDocumentGivenClass :: Document ->
                                Corpus ->
                                ClassificationName ->
                                Float
    probOfDocumentGivenClass
        doc
        corp@(Corpus
            {liklihood = liklihood,
            totalDocuments = total,
            numAtoms = num,
            stopWords = stopWords})
        name = prob + Map.fold (+) 0 nGramProb
          where
            ngrams     = NGram.fromString num stopWords doc
            cls        = findClassification name corp
            numClasses = length $ classificationNames corp
            prob       = liklihood (numDocuments cls) total numClasses
            nGramProb  = Map.mapWithKey mapper ngrams
            mapper k v = (fromIntegral v) * (log $ probOfNGram k name corp)

    getBestClassification :: Document -> Corpus -> (ClassificationName, Float)
    getBestClassification doc corp = foldl bestResult worstResult scores
      where
        worstResult     = ("No Match Found", -99999999.99)
        bestResult a b  = if (snd a) > (snd b)
            then a
            else b
        scores = getClassificationScores doc corp

    -- different types of liklihood
    maxLiklihood :: Int -> Int -> Int -> Float
    maxLiklihood x y _ = (fromIntegral x) / (fromIntegral y)

    laplacianLiklihood :: Int -> Int -> Int -> Float
    laplacianLiklihood x y k = (fromIntegral $ x + 1) / (fromIntegral $ y + k )


