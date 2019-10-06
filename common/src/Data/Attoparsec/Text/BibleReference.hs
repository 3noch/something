{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TypeFamilies #-}
module Data.Attoparsec.Text.BibleReference where

import Control.Applicative (liftA2, optional, (<|>))
import Data.Attoparsec.Text (Parser, asciiCI, char, choice, decimal, skipWhile)
import Data.Functor (($>))
import Data.Functor.Identity (Identity)
import Data.Text (Text)
import Data.Universe (universe)

import Data.Bible

ordinals :: [Parser Int]
ordinals =
  [ asciiCI "first"  $> 1
  , asciiCI "second" $> 2
  , asciiCI "third"  $> 3
  , asciiCI "fourth" $> 4
  , asciiCI "fifth"  $> 5
  ]

numericOrdinals :: [Parser Int]
numericOrdinals =
  [ asciiCI "1st" $> 1
  , asciiCI "2nd" $> 2
  , asciiCI "3rd" $> 3
  , asciiCI "4th" $> 4
  , asciiCI "5th" $> 5
  ]

romanNumerals :: [Parser Int]
romanNumerals =
  [ asciiCI "I"   $> 1
  , asciiCI "II"  $> 2
  , asciiCI "III" $> 3
  , asciiCI "IV"  $> 4
  , asciiCI "V"   $> 5
  ]

parseBookIndex :: Int -> Parser Int
parseBookIndex n
  =   choice (take n ordinals)
  <|> choice (take n romanNumerals)
  <|> choice (take n numericOrdinals)
  <|> decimal

parseVerse :: Parser (Int, Maybe Int)
parseVerse = do
  chap <- decimal
  verse <- optional $ char ':' *> decimal
  pure (chap, verse)

parseReference :: Parser (Canon' Identity, Maybe (Int, Maybe Int))
parseReference = liftA2 (,) parseCanonAny (skipStuff *> optional parseVerse)

parseTwo :: Parser Two
parseTwo = parseBookIndex 2 >>= \case
  1 -> pure Two_1
  2 -> pure Two_2
  _ -> fail "expected 1 or 2"

parseThree :: Parser Three
parseThree = parseBookIndex 3 >>= \case
  1 -> pure Three_1
  2 -> pure Three_2
  3 -> pure Three_3
  _ -> fail "expected 1 or 2 or 3"

skipStuff :: Parser ()
skipStuff = skipWhile (\c -> c == '.' || c == ' ')

bookAbbr :: [Text] -> Parser Text
bookAbbr xs = skipStuff *> choice (map asciiCI xs)

parseOldTestament :: OldTestament' Ignore -> Parser (OldTestament' Identity)
parseOldTestament = \case
  Genesis -> Genesis <$ bookAbbr ["Genesis","Gen","Ge","Gn"]
  Exodus -> Exodus <$ bookAbbr ["Exodus","Exod","Exo","Ex"]
  Leviticus -> Leviticus <$ bookAbbr ["Leviticus","Lev","Le","Lv"]
  Numbers -> Numbers <$ bookAbbr ["Numbers","Num","Nb","Nm","Nu"]
  Deuteronomy -> Deuteronomy <$ bookAbbr ["Deuteronomy","Deut","De","Dt"]
  Joshua -> Joshua <$ bookAbbr ["Joshua","Josh","Jos","Jsh"]
  Judges -> Judges <$ bookAbbr ["Judges","Jdgs","Judg","Jdg","Jg"]
  Ruth -> Ruth <$ bookAbbr ["Ruth","Rth","Ru"]
  Samuel () -> Samuel <$> parseTwo <* bookAbbr ["Samuel","Sam","Sa","Sm","S"]
  Kings () -> Kings <$> parseTwo <* bookAbbr ["Kings","Kgs","Kin","Ki","K"]
  Chronicles () -> Chronicles <$> parseTwo <* bookAbbr ["Chronicles","Chron","Chr","Ch"]
  Ezra -> Ezra <$ bookAbbr ["Ezra","Ezr","Ez"]
  Nehemiah -> Nehemiah <$ bookAbbr ["Nehemiah","Neh","Ne"]
  Esther -> Esther <$ bookAbbr ["Esther","Esth","Est","Es"]
  Job -> Job <$ bookAbbr ["Job","Jb"]
  Psalms -> Psalms <$ bookAbbr ["Psalms","Psalm","Pslm","Psa","Psm","Pss","Ps"]
  Proverbs -> Proverbs <$ bookAbbr ["Proverbs","Proverb","Prov","Pro","Prv","Pr"]
  Ecclesiastes -> Ecclesiastes <$ bookAbbr ["Ecclesiastes","Eccles","Eccle","Ecc","Qoh","Ec"]
  SongOfSolomon -> SongOfSolomon <$ bookAbbr ["Songs of Solomon","Song of Solomon","Canticle of Canticles","Song of Songs","Canticles","Songs","Cant","Song","SOS","So"]
  Isaiah -> Isaiah <$ bookAbbr ["Isaiah","Isa","Is"]
  Jeremiah -> Jeremiah <$ bookAbbr ["Jeremiah","Jer","Je","Jr"]
  Lamentations -> Lamentations <$ bookAbbr ["Lamentations","Lam","La"]
  Ezekiel -> Ezekiel <$ bookAbbr ["Ezekiel","Ezek","Eze","Ezk"]
  Daniel -> Daniel <$ bookAbbr ["Daniel","Dan","Da","Dn"]
  Hosea -> Hosea <$ bookAbbr ["Hosea","Hos","Ho"]
  Joel -> Joel <$ bookAbbr ["Joel","Jl"]
  Amos -> Amos <$ bookAbbr ["Amos","Am"]
  Obadiah -> Obadiah <$ bookAbbr ["Obadiah","Obad","Ob"]
  Jonah -> Jonah <$ bookAbbr ["Jonah","Jnh","Jon"]
  Micah -> Micah <$ bookAbbr ["Micah","Mic","Mc"]
  Nahum -> Nahum <$ bookAbbr ["Nahum","Nah","Na"]
  Habakkuk -> Habakkuk <$ bookAbbr ["Habakkuk","Hab","Hb"]
  Zephaniah -> Zephaniah <$ bookAbbr ["Zephaniah","Zeph","Zep","Zp"]
  Haggai -> Haggai <$ bookAbbr ["Haggai","Hag","Hg"]
  Zechariah -> Zechariah <$ bookAbbr ["Zechariah","Zech","Zec","Zc"]
  Malachi -> Malachi <$ bookAbbr ["Malachi","Mal","Ml"]

parseOldTestamentAny :: Parser (OldTestament' Identity)
parseOldTestamentAny = choice $ map parseOldTestament universe

parseNewTestament :: NewTestament' Ignore -> Parser (NewTestament' Identity)
parseNewTestament = \case
  Matthew -> Matthew <$ bookAbbr ["Matthew","Matt","Mt"]
  Mark -> Mark <$ bookAbbr ["Mark","Mar","Mrk","Mk","Mr"]
  Luke -> Luke <$ bookAbbr ["Luke","Luk","Lk"]
  John -> John <$ bookAbbr ["John","Jhn","Joh","Jn"]
  Acts -> Acts <$ bookAbbr ["Acts","Act","Ac"]
  Romans -> Romans <$ bookAbbr ["Romans","Rom","Rm","Ro"]
  Corinthians () -> Corinthians <$> parseTwo <* bookAbbr ["Corinthians","Cor","Co"]
  Galatians -> Galatians <$ bookAbbr ["Galatians","Gal","Ga"]
  Ephesians -> Ephesians <$ bookAbbr ["Ephesians","Ephes","Eph"]
  Philippians -> Philippians <$ bookAbbr ["Philippians","Phil","Php","Pp"]
  Colossians -> Colossians <$ bookAbbr ["Colossians","Col","Co"]
  Thessalonians () -> Thessalonians <$> parseTwo <* bookAbbr ["Thessalonians","Thess","Thes","Th"]
  Timothy () -> Timothy <$> parseTwo <* bookAbbr ["Timothy","Tim","Ti"]
  Titus -> Titus <$ bookAbbr ["Titus","Tit","Ti"]
  Philemon -> Philemon <$ bookAbbr ["Philemon","Philem","Phm","Pm"]
  Hebrews -> Hebrews <$ bookAbbr ["Hebrews","Heb"]
  James -> James <$ bookAbbr ["James","Jas","Jm"]
  Peter () -> Peter <$> parseTwo <* bookAbbr ["Peter","Pet","Pe","Pt","P"]
  JohnEpistle () -> JohnEpistle <$> parseThree <* bookAbbr ["John","Jhn","Joh","Jn","Jo","J"]
  Jude -> Jude <$ bookAbbr ["Jude","Jud","Jd"]
  Revelation -> Revelation <$ bookAbbr ["The Revelation","Revelation","Rev","Re"]

parseNewTestamentAny :: Parser (NewTestament' Identity)
parseNewTestamentAny = choice $ map parseNewTestament universe

parseCanonAny :: Parser (Canon' Identity)
parseCanonAny = Canon_Old <$> parseOldTestamentAny <|> Canon_New <$> parseNewTestamentAny

  -- ["Tob","Tb"]
  -- ["Jdth","Jdt","Jth"]
  -- ["The Rest of Esther","Rest of Esther","Add Esth","AddEsth","Add Es","AEs"]
  -- ["Wisd of Sol","Wisdom","Wis","Ws"]
  -- ["Sir"]
  -- ["Baruch","Ecclus","Bar"]
  -- ["Let Jer","Ltr Jer","Ep Jer","LJe"]
  -- ["The Song of the Three Holy Children","Song of the Three Holy Children","The Song of Three Youths","Song of Three Children","The Song of Three Jews","Song of Three Jews","Sg of 3 Childr","Song of Three","Song of Thr","Song Thr"]
  -- ["Azariah","Pr Az"]
  -- ["Sus"]
  -- ["Bel and Dr","Bel"]
  -- ["2 Maccabees","2Maccabees","Maccabees","2 Macc","2 Mac","2Macc","2Mac","Macc","2Ma","Mac","2M","Ma","M"]
  -- ["Esdras","Esdr","Esd","Es"]
  -- ["Prayer of Manasses","Pr of Man","PMa"]
  -- ["Add Psalm","Add Ps","Ode","de"]
  -- ["Psalms Solomon","Ps Solomon","Ps Sol","PsSol"]
  -- ["Epistle to Laodiceans","Epistle Laodiceans","Epist Laodiceans","Laodiceans","Ep Laod","Ep Lao","Laod"]
