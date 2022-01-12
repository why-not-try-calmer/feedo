module Search where

import AppTypes (Feed (f_items), Item (..), Field, KeyedItem (KeyedItem, key), FeedsSearch, SubChat (sub_settings, sub_feeds_links), Settings (settings_word_matches), match_searchset)
import Data.List (foldl')
import Data.SearchEngine
    ( NoFeatures,
      query,
      initSearchEngine,
      noFeatures,
      insertDocs,
      SearchConfig(..),
      SearchRankParameters(..) )
import qualified Data.Text as T
import NLP.Tokenize.Text
import qualified Data.Set as S

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
        xtract (KeyedItem _ i) _ = tokenize (i_title i) ++ tokenize (i_desc i)
        xform t _ = T.toCaseFold t

initSearchWith :: [Feed] -> ([KeyedItem], FeedsSearch)
initSearchWith feeds =
    let kitems = extractKItems feeds
    in (kitems, makeSearch kitems)
    where
        extractKItems :: [Feed] -> [KeyedItem]
        extractKItems fs = mapCount fs 0 []
        mapCount [] _ kitems = kitems
        mapCount (f:fs) !n !kitems =
            let items' = f_items f
                (!new_counter, !kitems') = foldl' (\(!c, !is) i -> (c+1, is ++ [KeyedItem (c+1) i])) (n, []) items'
            in  mapCount fs new_counter $ kitems ++ kitems'
        makeSearch :: [KeyedItem] -> FeedsSearch
        makeSearch items = insertDocs items $
            initSearchEngine
            defaultSearchConfig
            defaultSearchRankParameters

searchWith :: [KeyedItem] -> [T.Text] -> FeedsSearch -> [KeyedItem]
searchWith items q engine =
    let res = query engine q
    in  extractResults res items
    where
        extractResults _ [] = []
        extractResults indices is = map (sanitize is) indices
        sanitize is idx
            | idx == 1 = head is
            | idx == length is = last is
            | otherwise = let idx' = idx-1 in is !! idx'

scheduledSearches :: SubChat -> ([KeyedItem], FeedsSearch) -> [Item]
scheduledSearches c (keyed, engine) = 
    let searchset = match_searchset . settings_word_matches $ sub_settings c
        res = searchWith keyed (S.toList searchset) engine
        flinks = sub_feeds_links c
    in  foldl' (\acc (KeyedItem _ item) -> if i_feed_link item `S.member` flinks then item:acc else acc) [] res