module Search where

import Data.SearchEngine
import qualified Data.Text as T
import Data.Ix (Ix)
import AppTypes (Item(..), Feed (f_title, f_items, f_link))
import Data.List (foldl')

data KeyedItem = KeyedItem {
    key ::Int,
    item :: Item,
    feed_name :: T.Text,
    feed_link :: T.Text
} deriving (Eq, Show)

data Field = FieldTitle | FieldDescription deriving (Eq, Ord, Enum, Bounded, Ix, Show)

defaultSearchRankParameters :: SearchRankParameters Field NoFeatures
defaultSearchRankParameters =
    SearchRankParameters {
        paramK1 = 1.5,
        paramB = const 0.9,
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
        xtract (KeyedItem _ i _ _) _ = T.words $ i_title i `T.append` i_desc i
        xform t _ = T.toCaseFold t

type FeedsSearch = SearchEngine KeyedItem Int Field NoFeatures

prepareFeeds :: [Feed] -> [KeyedItem]
prepareFeeds feeds = mapCount feeds 0 []
    where
        mapCount [] _ kitems = kitems
        mapCount (f:fs) !n !kitems =
            let items = f_items f
                title = f_title f
                link = f_link f
                (new_counter, kitems') = foldl' (\(!c, !is) i -> (c+1, is ++ [KeyedItem (c+1) i title link])) (n, []) items
            in  mapCount fs new_counter $ kitems ++ kitems'

makeSearch :: [KeyedItem] -> FeedsSearch
makeSearch items = insertDocs items $ initSearchEngine defaultSearchConfig defaultSearchRankParameters

fetchResults :: [Int] -> [KeyedItem] -> [(T.Text, T.Text)]
fetchResults indices kitems = map ((\k -> (i_link . item $ k, i_title . item $ k)) . (kitems !!)) indices