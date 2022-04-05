{-# LANGUAGE StrictData #-}
{-# LANGUAGE OverloadedStrings #-}

module MarkdownChecker where

import Data.Foldable (foldl')
import Data.Maybe (fromMaybe)
import qualified Data.Text as T

{- Entities -}

singles :: [Char]
singles = ['_', '*', '~', '`', '|', '>', '#', '+', '-', '=', '.', '!']

doubles :: [Char]
doubles = ['[', '(', '{', '}', ')', ']']

openingDouble :: [Char]
openingDouble = take 3 doubles

falsePositives :: [Char]
falsePositives = drop 5 singles

closeOn :: [Char] -> Char -> Bool
closeOn [x] y
    | x == '(' && y == ')' = True
    | x == '[' && y == ']' = True
    | x `elem` singles && x == y = True
    | otherwise = False
closeOn [x, _] y
    | x == y = True
    | otherwise = False
closeOn ['`', '`', '`'] '`' = True
closeOn _ _ = False

encloseWith :: [Char] -> T.Text -> T.Text
encloseWith [x] txt
    | x == '(' = x' `T.append` txt `T.append` ")"
    | x == '[' = x' `T.append` txt `T.append` "]"
    | x == '{' = x' `T.append` txt `T.append` "}"
    | otherwise = x' `T.append` txt `T.append` x'
    where x' = T.singleton x
encloseWith [x, y] txt =
    let xy = T.pack [x, y]
    in  xy `T.append` txt `T.append` xy
encloseWith xs txt =
    let xs' = T.pack xs
    in  xs' `T.append` txt `T.append` xs'

{- Entities -}

data OC = Open | Closed deriving (Show, Eq)

data Entity =
    Root [Entity] |
    NonRep OC Char [Entity] |
    Rep OC ([Char], [Char]) [Entity] |
    Esc |
    Contents T.Text |
    TBD [Char] |
    Back |
    Up Entity |
    FailOn T.Text
    deriving (Show, Eq)

isEntity :: Char -> Maybe Entity
isEntity c
    | c `elem` ['_', '|', '`'] = Just $ TBD [c]
    | c `elem` singles = Just $ Rep Open ([c], []) []
    | c `elem` doubles = Just $ NonRep Open c []
    | otherwise = Nothing

mkEntity :: Char -> Entity
mkEntity c =
    let t = T.singleton c
    in  maybe (Contents t) (\e -> 
        if c `notElem` falsePositives then e
        else Contents t) $ isEntity c

detEntity :: Entity -> Char -> Either T.Text Entity
-- Either fails on an error or maybe return a new determined entity
detEntity (TBD ['_']) '_' = Right $ Rep Open (['_', '_'], []) []
detEntity (TBD ['_']) c = Right $ Rep Open (['_'], []) [Contents $ T.singleton c]
detEntity (TBD ['|']) '|' = Right $ Rep Open (['|', '|'], []) []
detEntity (TBD ['|']) c = Left $ "'" `T.append` T.singleton c `T.append` "' should have been escaped"
detEntity (TBD ['`']) '`' = Right $ TBD ['`', '`']
detEntity (TBD ['`']) c = Right $ Rep Open (['_'], []) [Contents $ T.singleton c]
detEntity (TBD ['`', '`']) '`' = Right $ Rep Open (['`', '`', '`'], []) []
detEntity (TBD ['`', '`']) c = Left $ "'" `T.append` T.singleton c `T.append` "' should have been escaped"
detEntity _ c = Left $ "Unable to determine anything from " `T.append` T.singleton c

{- Parsing -}

newtype Parser = Parser { runParser :: T.Text -> Either T.Text (Entity, Int) }

parse :: T.Text -> Either T.Text (Entity, Int)
parse = runParser . Parser $ \t -> runner . T.reverse $ t
    where
        runner = T.foldr step (Right (Root [], 0))
        step _ (Left err) = Left err
        step _ (Right (FailOn err, count)) = Left $ "Found error at character "
            `T.append` (T.pack . show $ count)
            `T.append` ": " `T.append` err
        step c (Right (entity, count)) = Right (parsing entity c, count + 1)

render :: Entity -> T.Text
render (Root children) = foldl' step mempty children
    where
        noFalsePositives text = T.foldl' (\t c ->
            if c `elem` falsePositives then t `T.append` "\\" `T.append` T.singleton c
            else t `T.append` T.singleton c) mempty text
        step res (NonRep _ '[' cs) = encloseWith ['['] (foldMap cont cs) `T.append` res
        step res (NonRep _ tag cs) = encloseWith [tag] (foldMap skipDirect cs) `T.append` res
        step res (Rep _ (ltags, _) cs) =
            let middle = if head ltags == '`' then foldMap skipDirect cs else foldMap cont cs
            in  encloseWith ltags middle `T.append` res
        step res (Contents text) = noFalsePositives text `T.append` res
        step res _ = res
        cont e = step mempty e
        skipDirect (Contents text) = text
        skipDirect e = cont e
render _ = mempty

parsing :: Entity -> Char -> Entity
parsing (FailOn err) _ = FailOn err
parsing (Root []) c =
    let seed = fromMaybe (Contents $ T.singleton c) $ isEntity c
    in  Root [seed]
parsing (Root (Esc:(Contents text):children)) c =
    let escaped = Contents $ text `T.append` T.singleton c
    in  Up $ Root (escaped:children)
parsing (Root (child:children)) c = case parsing child c of
    Up child' -> Root (child':children)
    Back -> Root (mkEntity c:child:children)
    FailOn err -> FailOn err
    _ -> undefined
parsing (Contents text) c =
    let contents = Up . Contents $ text `T.append` T.singleton c
    in  maybe contents (\_ -> if c `elem` falsePositives then contents else Back) $ isEntity c
parsing tba@(TBD _) c = case detEntity tba c of
    Left err -> FailOn err
    Right e -> Up e
parsing (Rep Closed _ _) _ = Back
parsing (Rep Open (['`'], r) []) c = Up $ Rep Open (['`'], r) [Contents $ T.singleton c]
parsing (Rep Open (['`'], r) (Contents text:children)) c
    | c == '`' = Up $ Rep Closed (['`'], [c]) (Contents text:children)
    | otherwise = Up $ Rep Open (['`'], r) (upped:children)
    where upped = Contents $ text `T.append` T.singleton c
parsing (Rep Open (['`','`','`'], r) []) c = Up $ Rep Open (['`','`','`'], r) [Contents $ T.singleton c]
parsing (Rep Open tags []) c = Up $ Rep Open tags [mkEntity c]
parsing (Rep Open (ltags@['`','`','`'], rtags) (Contents text:children)) c
    | ltags == rtags' = Up $ Rep Closed (ltags, rtags') (Contents text:children)
    | c == '`' = Up $ Rep Open (ltags, rtags') (Contents text:children)
    | otherwise = Up $ Rep Open (ltags, rtags) (upped:children)
    where
        upped = Contents $ text `T.append` T.singleton c
        rtags' = c:rtags
parsing (Rep Open tags (Esc:(Contents text):children)) c =
    let escaped = Contents $ text `T.append` T.singleton c
    in  Up $ Rep Open tags (escaped:children)
parsing (Rep Open (ltags, rtags) (child:children)) c = case parsing child c of
    Up child' -> Up $ Rep Open (ltags, rtags) (child':children)
    Back ->
        if not (ltags `closeOn` c) then Up $ Rep Open (ltags, rtags) (mkEntity c:child:children)
        else let rtags' = c:rtags in
            if ltags == rtags' then Up $ Rep Closed (ltags, rtags') (child:children)
            else Up $ Rep Open (ltags, rtags') (child:children)
    FailOn txt -> FailOn txt
    _ -> undefined
parsing (NonRep Closed _ _) _ = Back
parsing (NonRep Open tag []) c = Up $ NonRep Open tag [mkEntity c]
parsing (NonRep Open tag (Esc:(Contents text):children)) c =
    let escaped = Contents $ text `T.append` T.singleton c
    in  Up $ NonRep Open tag (escaped:children)
parsing (NonRep Open tag (child:children)) c = case parsing child c of
    Up child' -> Up $ NonRep Open tag (child':children)
    Back ->
        if [tag] `closeOn` c then Up $ NonRep Closed tag (child:children)
        else Up $ NonRep Open tag (mkEntity c:child:children)
    FailOn err -> FailOn err
    _ -> undefined
parsing e _ = e