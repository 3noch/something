{-# LANGUAGE EmptyCase #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE TemplateHaskell #-}

module Common.Route where

import Control.Monad.Except (MonadError (throwError))
import Data.Attoparsec.Text (endOfInput, parseOnly)
import Data.Functor.Identity (Identity)
import Data.Text (Text)
import qualified Data.Text as T

import Obelisk.Route
import Obelisk.Route.TH (deriveRouteComponent)

import Common.Prelude
import Data.Bible (Canon, Canon' (..), newTestamentToString, oldTestamentToString)
import Data.Attoparsec.Text.BibleReference (parseReference)

data BackendRoute :: * -> * where
  -- | Used to handle unparseable routes.
  BackendRoute_Missing :: BackendRoute ()
  BackendRoute_Listen :: BackendRoute ()
  -- You can define any routes that will be handled specially by the backend here.
  -- i.e. These do not serve the frontend, but do something different, such as serving static files.

data FrontendRoute :: * -> * where
  FrontendRoute_Main :: FrontendRoute ()
  FrontendRoute_Reference :: FrontendRoute (Canon, Maybe (Int, Maybe Int))
  -- This type is used to define frontend routes, i.e. ones for which the backend will serve the frontend.

fullRouteEncoder :: Encoder (Either Text) Identity (R (FullRoute BackendRoute FrontendRoute)) PageName
fullRouteEncoder = mkFullRouteEncoder
  (FullRoute_Backend BackendRoute_Missing :/ ())
  (\case
    BackendRoute_Missing -> PathSegment "missing" $ unitEncoder mempty
    BackendRoute_Listen -> PathSegment "listen" $ unitEncoder mempty
  )
  (\case
    FrontendRoute_Main -> PathEnd $ unitEncoder mempty
    FrontendRoute_Reference -> PathSegment "at" $ bibleReferenceEncoder >>> singlePathSegmentEncoder
  )

checkedFullRouteEncoder :: Encoder Identity Identity (R (FullRoute BackendRoute FrontendRoute)) PageName
checkedFullRouteEncoder = case checkEncoder fullRouteEncoder of
  Left e -> error (show e)
  Right x -> x

bibleReferenceEncoder :: (Applicative check, MonadError Text parse) => Encoder check parse (Canon, Maybe (Int, Maybe Int)) Text
bibleReferenceEncoder = unsafeMkEncoder $ EncoderImpl
  { _encoderImpl_decode = \input -> case parseBibleReference input of
    Right x -> pure x
    Left e -> throwError $ "Failed to understand Bible reference " <> input <> ": " <> e
  , _encoderImpl_encode = T.pack . showBibleReference
  }

parseBibleReference :: Text -> Either Text (Canon, Maybe (Int, Maybe Int))
parseBibleReference input = first T.pack $ parseOnly (parseReference <* endOfInput) (T.strip input)

showBibleReference :: (Canon, Maybe (Int, Maybe Int)) -> String
showBibleReference (b, chapVerse) = bookName <> maybe "" showChapVerse chapVerse
  where
    showChapVerse (chap, verse') = show chap <> maybe "" ((':' :) . show) verse'
    bookName = case b of
      Canon_Old x -> oldTestamentToString showEnum x
      Canon_New x -> newTestamentToString showEnum showEnum x

    showEnum :: Enum a => a -> String -> String
    showEnum n name = show (fromEnum n + 1) <> " " <> name

concat <$> traverse deriveRouteComponent
  [ ''BackendRoute
  , ''FrontendRoute
  ]
