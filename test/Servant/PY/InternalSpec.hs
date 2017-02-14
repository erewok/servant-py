{-# LANGUAGE DataKinds           #-}
{-# LANGUAGE FlexibleInstances   #-}
{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE QuasiQuotes         #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeFamilies        #-}
{-# LANGUAGE TypeOperators       #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}

module Servant.PY.InternalSpec where

import           Data.Either                                (isRight)
import           Data.Monoid                                ()
import           Data.Monoid.Compat                         ((<>))
import           Data.Proxy
import           Data.Text                                  (Text)
import qualified Data.Text                                  as T
import           GHC.TypeLits
import           Prelude                                    ()
import           Prelude.Compat
import           Test.Hspec                                 hiding
                                                             (shouldContain,
                                                             shouldNotContain)
import           Test.QuickCheck                            (Arbitrary (..),
                                                             choose, listOf,
                                                             property)

import           Servant.API.ContentTypes
import           Servant.API.Internal.Test.ComprehensiveAPI

import           Servant.PY.Internal


customOptions :: CommonGeneratorOptions
customOptions = defCommonGeneratorOptions
 { urlPrefix = "urlForRequesting:9000"
 , returnMode = DangerMode
 }

spec :: Spec
spec = describe "Servant.PY.Internal" internalSpec

shouldContain :: Text -> Text -> Expectation
a `shouldContain` b  = shouldSatisfy a (T.isInfixOf b)

shouldNotContain :: Text -> Text -> Expectation
a `shouldNotContain` b  = shouldNotSatisfy a (T.isInfixOf b)

newtype ASCII = ASCII {getASCII :: T.Text} deriving (Show)

instance Arbitrary ASCII where
   -- Our arbitrary instance is generating only ASCII, since the language-ecmascript's lexer
   -- is currently (October 2016) still a bit naïve
   arbitrary = fmap (ASCII . T.pack) $ listOf $ choose (minBound, '\127')
   shrink xs = (ASCII . T.pack) <$> shrink (T.unpack $ getASCII xs)


internalSpec :: Spec
internalSpec = describe "Internal" $ do
    it "should only indent using whitespace" $
      property $ \n -> indenter n indent == mconcat (replicate n (T.pack " "))

    -- it "should generate only valid python identifiers for any ASCII route" $ do
    --     let parseIdentifier = fmap T.pack ""
    --     property $ \x -> let valid = toValidFunctionName $ getASCII x in
    --                      Right valid == parseIdentifier valid
    --
    -- it "should generate a valid python identifier when supplied with hyphens, unicode whitespace, non-bmp unicode" $ do
    --     toValidFunctionName "a_--a\66352b\6158c\65075" `shouldBe` "a_abc\65075"
