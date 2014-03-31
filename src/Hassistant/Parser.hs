{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE NoMonomorphismRestriction #-}
{-# LANGUAGE LambdaCase #-}

module Hassistant.Parser where

import Control.Applicative

import qualified Data.Attoparsec.Text as A
import qualified Data.Char as Char
import qualified Data.Text as T

space :: A.Parser T.Text
space = A.takeWhile Char.isSpace

moduleP' :: A.Parser T.Text
moduleP' = A.string "module" *> A.takeWhile1 Char.isSpace *> A.takeWhile1 (A.inClass "a-zA-Z0-9.")

moduleP :: A.Parser T.Text
moduleP = moduleP' <|> takeLine *> next
  where takeLine = A.takeWhile (not . A.isEndOfLine)
        next     = A.endOfLine *> moduleP' <|> A.endOfLine *> takeLine *> next

dropCommentP :: A.Parser String
dropCommentP = block <|> ("" <$ A.string "--") <|> (A.anyChar >>= \c -> (c:) <$> dropCommentP) <|> pure ""
  where block = A.string "{-" *> A.manyTill A.anyChar (A.string "-}") *> ((' ':) <$> dropCommentP)

positionLanguageP :: A.Parser Int
positionLanguageP = do
    space1 <- T.length <$> space
    _      <- A.string "{-#"
    space2 <- T.length <$> space
    _      <- A.string "LANGUAGE"
    space3 <- T.length <$> A.takeWhile1 Char.isSpace
    lastP  <- fst <$> lastOfList (A.takeWhile1 $ A.inClass "A-Za-z") ','
    return $ 11 + space1 + space2 + space3 + lastP

class Sized a where
  size :: a -> Int

instance Sized T.Text where
    size = T.length

lastOfList :: Sized a => A.Parser a -> Char -> A.Parser (Int, a)
lastOfList bdy sep = ((,,) <$> space <*> bdy <*> space) `A.sepBy` A.char sep >>= \case
    []  -> fail "lastOfList: unknown error."
    [(as,p,_)] -> return (size as, p)
    ps  -> do
        let (as,p,_) = last ps
            bf       = sum . map (succ . tripleLen) $ init ps
        return (bf + T.length as, p)
  where
    tripleLen (a,b,c) = T.length a + size b + T.length c

positionModuleP :: A.Parser Int
positionModuleP = do
    _      <- A.string "import"
    space1 <- T.length <$> A.takeWhile1 Char.isSpace
    qual   <- A.option 0 $ (+) 
                <$> (9 <$ A.string "qualified")
                <*> (T.length <$> A.takeWhile1 Char.isSpace)
    pkg    <- A.option 0 $ (+)
                <$> ((+2) . length <$> (A.char '"' *> A.manyTill A.anyChar (A.char '"')))
                <*> (T.length <$> A.takeWhile1 Char.isSpace)
    return $ 6 + space1 + qual + pkg

moduleNameP :: A.Parser T.Text
moduleNameP = T.intercalate "." <$> mdl `A.sepBy1` A.char '.'
  where
    mdl = T.cons <$> A.satisfy (A.inClass "A-Z") <*> A.takeWhile (A.inClass "A-Za-z0-9#'")

positionNamesInModuleP' :: A.Parser (T.Text, ImportElem, Int)
positionNamesInModuleP' = do
    imp    <- positionModuleP
    space1 <- T.length <$> space
    mName  <- moduleNameP
    space2 <- T.length <$> space
    as     <- A.option 0 $ do
        _      <- A.string "as"
        space3 <- T.length <$> A.takeWhile1 Char.isSpace
        _      <- A.satisfy (A.inClass "A-Z")
        other  <- T.length <$> A.takeWhile (A.inClass "A-Za-z0-9#")
        return $ 3 + space3 + other
    space3 <- T.length <$> space
    _      <- A.char '('
    (lp,w) <- lastOfList importElem ','
    return (mName, w, imp + space1 + T.length mName + space2 + as + space3 + lp + 1)

variable :: A.Parser T.Text
variable = T.cons <$> (A.satisfy (A.inClass "a-zA-Z")) <*> A.takeWhile (A.inClass "a-zA-Z0-9'")

infixVar :: A.Parser T.Text
infixVar = do
    op  <- A.char '('
    bdy <- A.takeWhile (/= ')')
    cl  <- A.char ')'
    return $ op `T.cons` bdy `T.snoc` cl

data ImportElem =
    ImportElem { ieParent   :: T.Text
               , ieLastElem :: T.Text
               , ieSize     :: Int
               } deriving Show

instance Sized ImportElem where
    size ImportElem{ieSize = s} = s

importElem :: A.Parser ImportElem
importElem = do
    parent   <- variable <|> infixVar
    space1   <- T.length <$> space
    (el,elm) <- A.option (0, "") $ do
        _ <- A.char '('
        space2   <- T.length <$> space
        (lp,w)   <- lastOfList (variable <|> infixVar) ','
        space3   <- T.length <$> space
        _ <- A.char ')'
        return (2 + space2 + space3 + lp + T.length w, w)
    return $ ImportElem parent elm (T.length parent + space1 + el)

positionNamesInModuleP :: A.Parser (T.Text, Int)
positionNamesInModuleP = (\(m,_,i) -> (m,i)) <$> positionNamesInModuleP'

positionNamesInConstructorP :: A.Parser (T.Text, T.Text, Int)
positionNamesInConstructorP = do
    (mdl,ie,pos) <- positionNamesInModuleP'
    space1       <- T.length <$> space
    _            <- A.char '('
    space2       <- T.length <$> space
    (lp,_)       <- lastOfList (A.takeWhile (/= ',')) ','
    return (mdl, ieParent ie, size ie + pos + space1 + space2 + lp + 1)

