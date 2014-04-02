{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE LambdaCase #-}

module Hassistant.Parser where

import Control.Applicative

import qualified Data.Attoparsec.Text as A
import qualified Data.Char as Char
import qualified Data.Text as T

import Hassistant.LANGUAGE

isHsSymbol :: Char -> Bool
isHsSymbol c = not special && sym
  where special = A.inClass "(),;[]`{}_:\"'" c
        sym     = Char.isSymbol c || Char.isPunctuation c

isHsLower :: Char -> Bool
isHsLower c = Char.isLower c || c == '_'

qualified :: A.Parser T.Text -> A.Parser T.Text
qualified p = q <|> p
  where 
    q = do
        mdl <- moduleNameP
        optional (A.char '.' *> p) >>= return . \case
            Nothing -> mdl
            Just b  -> mdl `T.append` ('.' `T.cons` b)

varid :: A.Parser T.Text
varid = T.cons <$> A.satisfy isHsLower <*> A.takeWhile p
  where
    p c = Char.isUpper c || isHsLower c || Char.isDigit c || (c == '\'')

conid :: A.Parser T.Text
conid = T.cons <$> A.satisfy Char.isUpper <*> A.takeWhile p
  where
    p c = Char.isUpper c || isHsLower c || Char.isDigit c || (c == '\'')

space :: A.Parser Int
space = T.length <$> A.takeWhile Char.isSpace

space1 :: A.Parser Int
space1 = T.length <$> A.takeWhile1 Char.isSpace

seekTopLevel :: A.Parser a -> A.Parser a
seekTopLevel p = p <|> dropLine *> next
  where
    dropLine = A.skipWhile (not . A.isEndOfLine)
    next     = A.endOfLine *> p <|> A.endOfLine *> dropLine *> next

moduleNameP :: A.Parser T.Text
moduleNameP = T.intercalate "." <$> conid `A.sepBy1` A.char '.'

moduleP' :: A.Parser T.Text
moduleP' = A.string "module" *> A.takeWhile1 Char.isSpace *> moduleNameP

moduleP :: A.Parser T.Text
moduleP = seekTopLevel moduleP'

dropCommentP :: A.Parser String
dropCommentP = block <|> ("" <$ A.string "--") <|> (A.anyChar >>= \c -> (c:) <$> dropCommentP) <|> pure ""
  where block = A.string "{-" *> A.manyTill A.anyChar (A.string "-}") *> ((' ':) <$> dropCommentP)

positionLanguageP :: A.Parser Int
positionLanguageP = do
    sp1 <- space
    _   <- A.string "{-#"
    sp2 <- space
    _   <- A.string "LANGUAGE"
    sp3 <- space1
    lP  <- fst <$> lastOfList (A.takeWhile1 $ flip elem languageChar) ','
    return $ 11 + sp1 + sp2 + sp3 + lP

class Sized a where
  size :: a -> Int

instance Sized T.Text where
    size = T.length

lastOfList :: Sized a => A.Parser a -> Char -> A.Parser (Int, a)
lastOfList bdy sep = ((,,) <$> space <*> bdy <*> space) `A.sepBy` A.char sep >>= \case
    []  -> fail "lastOfList: unknown error."
    [(as,p,_)] -> return (as, p)
    ps  -> do
        let (as,p,_) = last ps
            bf       = sum . map (succ . tripleLen) $ init ps
        return (bf + as, p)
  where
    tripleLen (a,b,c) = a + size b + c

positionModuleP :: A.Parser Int
positionModuleP = do
    _    <- A.string "import"
    sp1  <- space1
    qual <- A.option 0 $ (+) <$> (9 <$ A.string "qualified") <*> space1
    pkg  <- A.option 0 $ (+) <$> inParen                     <*> space1
    return $ 6 + sp1 + qual + pkg
  where
    inParen = (+2) . length <$> (A.char '"' *> A.manyTill A.anyChar (A.char '"'))

data ImportElem =
    ImportElem { ieParent   :: T.Text
               , ieLastElem :: T.Text
               , ieSize     :: Int
               } deriving Show

instance Sized ImportElem where
    size ImportElem{ieSize = s} = s

importElem :: A.Parser ImportElem
importElem = do
    prt <- elm
    sp1 <- space
    (el,wrd) <- A.option (0, "") $ do
        _      <- A.char '('
        sp2    <- space
        (lp,w) <- lastOfList elm ','
        sp3    <- space
        _      <- A.char ')'
        return (2 + sp2 + sp3 + lp + T.length w, w)
    return $ ImportElem prt wrd (T.length prt + sp1 + el)
  where
    elm = conid <|> varid <|> infixVar
    infixVar = do
        op  <- A.char '('
        bdy <- A.takeWhile isHsSymbol
        cl  <- A.char ')'
        return $ op `T.cons` bdy `T.snoc` cl

positionNamesInModuleP' :: A.Parser (T.Text, ImportElem, Int)
positionNamesInModuleP' = do
    imp    <- positionModuleP
    sp1    <- space
    mName  <- moduleNameP
    sp2    <- space
    as     <- A.option 0 asElem
    sp3    <- space
    _      <- A.char '('
    (lp,w) <- lastOfList importElem ','
    return (mName, w, imp + sp1 + T.length mName + sp2 + as + sp3 + lp + 1)
  where
    asElem = do
        _     <- A.string "as"
        sp3   <- space1
        con   <- (T.length <$> conid)
        return $ 2 + sp3 + con

positionNamesInModuleP :: A.Parser (T.Text, Int)
positionNamesInModuleP = (\(m,_,i) -> (m,i)) <$> positionNamesInModuleP'

positionNamesInConstructorP :: A.Parser (T.Text, T.Text, Int)
positionNamesInConstructorP = do
    (mdl,ie,pos) <- positionNamesInModuleP'
    sp1          <- space
    _            <- A.char '('
    sp2          <- space
    (lp,_)       <- lastOfList (A.takeWhile (/= ',')) ','
    return (mdl, ieParent ie, size ie + pos + sp1 + sp2 + lp + 1)

positionTopLevelP :: A.Parser ()
positionTopLevelP = A.skipWhile (A.inClass "a-z") *> A.endOfInput

hsLexP :: A.Parser [(Int, T.Text)]
hsLexP = many $ (,) <$> space <*> token
  where
    sp    = T.singleton <$> (A.satisfy (A.inClass "(),;[]`{}"))
    resop = A.choice . map A.string $ 
        [ "..", "::", ":" , "=", "\\", "|", "<-", "->", "@", "~" ]

    paren st bd ed = (\a b c -> a `T.cons` b `T.snoc` c) <$> A.char st <*> bd <*> A.char ed

    token = qualified (varid <|> conid <|> sym <|> cSym <|> par sym <|> par cSym) <|> lit <|> sp <|> resop 
    par p = (\a -> '(' `T.cons` a `T.snoc` ')') <$> (A.char '(' *> p <* A.char ')')
    sym   = A.takeWhile1 isHsSymbol
    cSym  = T.cons <$> A.char ':' <*> sym

    lit   = num <|> char <|> str
    num   = A.takeWhile1 (A.inClass "-0-9.+eE")
    char  = paren '\'' (T.singleton <$> A.anyChar) '\''
    str   = do
        op <- A.char '"'
        bd <- many $ A.string "\\\"" <|> (T.singleton <$> A.notChar '"')
        ed <- A.char '"'
        return $ op `T.cons` T.concat bd `T.snoc` ed

positionOtherP :: A.Parser (Bool, Int)
positionOtherP = do
    (bf,_):is <- reverse <$> hsLexP
    let typ = "::" `elem` map snd is
    return (typ, sum (map len is) + bf)
  where 
    len (bf,b) = bf + T.length b

