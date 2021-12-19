module Search where

import AppTypes (Feed (f_items, f_link, f_title), Item (..))
import Data.Ix (Ix)
import Data.List (foldl')
import Data.SearchEngine
import qualified Data.Text as T

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
        xtract (KeyedItem _ i _ _) _ = T.words $ i_title i `T.append` " " `T.append` i_desc i
        xform t _ = T.toCaseFold t

type FeedsSearch = SearchEngine KeyedItem Int Field NoFeatures

initSearchWith :: [Feed] -> ([Item], FeedsSearch)
initSearchWith feeds = 
    let (items, kitems) = extractKItems feeds
    in  (items, makeSearch kitems)
    where
        extractKItems :: [Feed] -> ([Item], [KeyedItem])
        extractKItems fs = mapCount fs 0 ([], [])
        mapCount [] _ (items, kitems) = (items, kitems)
        mapCount (f:fs) !n (!items, !kitems) =
            let items' = f_items f
                title = f_title f
                link = f_link f
                (!new_counter, !kitems') = foldl' (\(!c, !is) i -> (c+1, is ++ [KeyedItem (c+1) i title link])) (n, []) items'
            in  mapCount fs new_counter (items ++ items', kitems ++ kitems')
        makeSearch :: [KeyedItem] -> FeedsSearch
        makeSearch items = insertDocs items $ 
            initSearchEngine
            defaultSearchConfig
            defaultSearchRankParameters

searchWith :: [Item] -> [T.Text] -> FeedsSearch -> [Item]
searchWith items q engine = 
    let res = query engine q
    in  extractResults res items 
    where
    extractResults :: [Int] -> [Item] -> [Item]
    extractResults indices is = map (is !!) indices