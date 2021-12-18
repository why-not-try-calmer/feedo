module Search where

import Data.SearchEngine
import qualified Data.Text as T
import Data.Ix (Ix)
import AppTypes (Item(..))
import Data.List (foldl')

data KeyedItem = KeyedItem {
    key ::Int,
    item :: Item
}

data Field = FieldTitle | FieldDescription deriving (Eq, Ord, Enum, Bounded, Ix, Show)

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

defaultSearchConfig :: SearchConfig KeyedItem Int Field NoFeatures
defaultSearchConfig =
    SearchConfig    {
        documentKey = key,
        extractDocumentTerms = xtract,
        transformQueryTerm = xform,
        documentFeatureValue = const noFeatures
    }
    where
        xtract (KeyedItem _ i) _ = T.words $ i_title i `T.append` i_desc i
        xform t _ = T.toCaseFold t

type FeedsSearch = SearchEngine KeyedItem Int Field NoFeatures

makeSearch :: [Item] -> FeedsSearch
makeSearch items = insertDocs keyed $ initSearchEngine defaultSearchConfig defaultSearchRankParameters
    where
        keyed = snd $ foldl' (\(!c, !is) i -> (c+1, is ++ [KeyedItem (c+1) i])) (-1,[]) items

fetchResults :: [Int] -> [Item] -> [Item]
fetchResults indices items = map (items !!) indices