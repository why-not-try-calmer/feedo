module Search where

import Data.SearchEngine
import qualified Data.Text as T
import Data.Ix (Ix)

data MockDoc = MockDoc {
    ref :: Int,
    contents :: T.Text
}

data Field = Ref deriving (Eq, Ord, Enum, Bounded, Ix, Show)

someDocs :: [MockDoc]
someDocs =
    let toTxt n = T.pack . show $ n
    in  map (\n -> MockDoc n ("val " `T.append` toTxt (n+1))) ([0..5] :: [Int])

defaultSearchRankParameters :: SearchRankParameters Field NoFeatures
defaultSearchRankParameters =
    SearchRankParameters {
        paramK1 = 1.2,
        paramB = const 0.75,
        paramFieldWeights = const 20,
        paramFeatureWeights = noFeatures,
        paramFeatureFunctions = noFeatures,
        paramResultsetSoftLimit = 200,
        paramResultsetHardLimit = 400,
        paramAutosuggestPrefilterLimit  = 500,
        paramAutosuggestPostfilterLimit = 500
    }

defaultSearchConfig :: SearchConfig MockDoc Int Field NoFeatures
defaultSearchConfig = 
    SearchConfig    {
        documentKey = ref,
        extractDocumentTerms = extractField,
        transformQueryTerm = const,
        documentFeatureValue = const noFeatures
    }
    where 
        extractField doc field = {-case field of Ref ->-} T.words . contents $ doc

loadSearchEngine :: SearchEngine MockDoc Int Field NoFeatures
loadSearchEngine = insertDocs someDocs $ 
    initSearchEngine defaultSearchConfig defaultSearchRankParameters