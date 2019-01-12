{-# LANGUAGE DataKinds           #-}
{-# LANGUAGE DeriveGeneric       #-}
{-# LANGUAGE FlexibleInstances   #-}
{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeFamilies        #-}
{-# LANGUAGE TypeOperators       #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}

module Servant.PY.InternalSpec where

import           Control.Lens
import           Data.Aeson
import qualified Data.ByteString.Char8                      as B
import           Data.Monoid
import           Data.Proxy
import           Data.Text                                  (Text)
import qualified Data.Text                                  as T
import           GHC.Generics
import           Prelude                                    ()
import           Prelude.Compat
import           Test.Hspec                                 hiding
                                                             (shouldContain,
                                                             shouldNotContain)
import           Test.QuickCheck                            (Arbitrary (..),
                                                             choose, listOf,
                                                             property)

import           Servant.API.ContentTypes
import           Servant.API.Internal.Test.ComprehensiveAPI()
import           Servant.Foreign

import           Servant.PY.Internal


data SomeJson = SomeJson
 { uvalue       :: !T.Text
 , pvalue       :: !T.Text
 , otherMissing :: Maybe T.Text
 } deriving (Eq, Show, Generic)
instance ToJSON SomeJson

-- * Our API type
type TestApi = "counter-req-header" :> Post '[JSON] SomeJson
          :<|> "counter-queryparam"
            :> QueryParam "sortby" T.Text
            :> Header "Some-Header" T.Text :> Get '[JSON] SomeJson
          :<|> "login-queryflag" :> QueryFlag "published" :> Get '[JSON] SomeJson
          :<|> "login-params-authors-with-reqBody"
            :> QueryParams "authors" T.Text
            :> ReqBody '[JSON] SomeJson :> Post '[JSON] SomeJson

testApi :: Proxy TestApi
testApi = Proxy


type CaptureApi = "login-with-path-var-and-header"
                  :> Capture "id" Int
                  :> Capture "Name" T.Text
                  :> Capture "hungrig" Bool
                  :> ReqBody '[JSON] SomeJson
                  :> Post '[JSON] (Headers '[Header "test-head" B.ByteString] SomeJson)
captureApi :: Proxy CaptureApi
captureApi = Proxy

customOptions :: CommonGeneratorOptions
customOptions = defCommonGeneratorOptions
 { urlPrefix = "urlForRequesting:9000"
 , returnMode = RawResponse
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
  describe "pure text functions" $ do
    it "should only indent using whitespace" $
      property $ \n -> indenter n indent == mconcat (replicate n (T.pack " "))

    it "should generate a valid python identifier when supplied with hyphens, unicode whitespace, non-bmp unicode" $
      toValidFunctionName "a_--a\66352b\6158c\65075" `shouldBe` "a_abc\65075"

    it "should produce PyDicts where the key is a quoted version of the variable name" $ do
      let dict = toPyDict "  " ["forty", "one", "people"]
      dict `shouldBe` "{\"forty\": forty,\n  \"one\": one,\n  \"people\": people}"

  describe "functions that operate on Req objects" $ do
    let captureList = listFromAPI (Proxy :: Proxy NoTypes) (Proxy :: Proxy NoContent) captureApi
    it "should correctly find captures" $ do
      let captured = captures . head $ map UnTypedPythonRequest captureList
      captured `shouldBe` ["id", "Name", "hungrig"]

    let reqList = listFromAPI (Proxy :: Proxy NoTypes) (Proxy :: Proxy NoContent) testApi
    it "should not incorrectly find captures" $ do
      let captured = captures . head $ map UnTypedPythonRequest reqList
      captured `shouldBe` []

    let req = head captureList
    let pathParts = req ^.. reqUrl.path.traverse
    it "should correctly find captures as a list" $
      capturesToFormatArgs pathParts `shouldBe` ["id", "Name", "hungrig"]

    it "should correctly format captures" $
      withFormattedCaptures "  " pathParts `shouldBe` ".format(\n  id=parse.quote(str(id)),\n"
                                                    <> "  Name=parse.quote(str(Name)),\n  "
                                                    <> "hungrig=parse.quote(str(hungrig)))"

    it "should build a formatted val with parse.quote and str" $
      property $ \s -> T.isInfixOf "=parse.quote(str(" $ formatBuilder $ T.pack s
    it "should build a formatted val that ends with parens" $
      property $ \s -> T.isSuffixOf (T.pack s <> "))") $ formatBuilder $ T.pack s

    it "should build urls properly with / separator" $ do
      let pyUrl = makePyUrl customOptions (UnTypedPythonRequest req) " "
      pyUrl `shouldBe` "\"urlForRequesting:9000/login-with-path-var-and-header/{id}/{Name}/{hungrig}\""
                       <> withFormattedCaptures " " pathParts

    it "should do segment-to-str as a plain string for Static" $
      segmentToStr (head pathParts) == "login-with-path-var-and-header"
    it "should do segment-to-str in formatting braces for a capture" $
      segmentToStr (last pathParts) == "{hungrig}"
    it "should build a doctstring that looks like a regular Python docstring" $ do
      let docstring = buildDocString (UnTypedPythonRequest req) customOptions ""
      docstring `shouldContain` "POST"
      docstring `shouldContain` "/login-with-path-var-and-header/{id}/{Name}/{hungrig}"
      docstring `shouldContain` "Args:"
      docstring `shouldContain` "Returns:"
