{-# LANGUAGE DataKinds           #-}
{-# LANGUAGE FlexibleInstances   #-}
{-# LANGUAGE QuasiQuotes         #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeFamilies        #-}
{-# LANGUAGE TypeOperators       #-}
{-# LANGUAGE OverloadedStrings   #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}

module Servant.PYSpec where

import           Data.Either                  (isRight)
import           Data.Monoid                  ()
import           Data.Monoid.Compat           ((<>))
import           Data.Proxy
import           Data.Text                    (Text)
import qualified Data.Text                    as T
import           Prelude                      ()
import           Prelude.Compat
import           Test.Hspec  hiding (shouldContain, shouldNotContain)
import           Test.QuickCheck              (Arbitrary (..),
                                               choose, listOf,
                                               property)

import           Servant.API.Internal.Test.ComprehensiveAPI
import           Servant.API.ContentTypes
import           Servant.PY
import           Servant.PY.Internal
import qualified Servant.PY.Requests           as R
import Servant.Custom.PYHeaders



-- This declaration simply checks that all instances are in place.
_ = pyForAPI comprehensiveAPIWithoutRaw requests :: Text

-- * specs

type TestAPI = "simple" :> ReqBody '[JSON,FormUrlEncoded] Text :> Post '[JSON] Bool
          :<|> "has.extension" :> Get '[FormUrlEncoded,JSON] Bool

type TopLevelRawAPI = "test" :> Get '[JSON] Int
                      :<|> Raw

type HeaderHandlingAPI = "test" :> Header "Foo" Text
                                :> Get '[JSON] Int

type CustomAuthAPI = "test" :> Authorization "Basic" Text
                            :> Get '[JSON] Int

type CustomHeaderAPI = "test" :> ThreeMilePilot Text
                              :> Get '[JSON] Int

type CustomHeaderAPI2 = "test" :> KlikatatIkatowi Text
                               :> Get '[JSON] Int

headerHandlingProxy :: Proxy HeaderHandlingAPI
headerHandlingProxy = Proxy

customAuthProxy :: Proxy CustomAuthAPI
customAuthProxy = Proxy

customHeaderProxy :: Proxy CustomHeaderAPI
customHeaderProxy = Proxy

customHeaderProxy2 :: Proxy CustomHeaderAPI2
customHeaderProxy2 = Proxy

data TestNames = Requests
               | RequestsWith
                 deriving (Show, Eq)

customOptions :: CommonGeneratorOptions
customOptions = defCommonGeneratorOptions
  { urlPrefix = "urlForRequesting:9000"
  , returnMode = DangerMode
  }

spec :: Spec
spec = describe "Servant.Requests" $ do
    generatePYSpec Requests       R.requestsWithDef
    generatePYSpec RequestsWith  (R.requestsWith customOptions)
    internalSpec

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
    it "should generate only valid javascript identifiers for any ASCII route" $ do
        let parseIdentifier = fmap T.pack. parse identifier ""
        property $ \x -> let valid = toValidFunctionName $ getASCII x in
                         Right valid == parseIdentifier valid

    it "should generate a valid javascript identifier when supplied with hyphens, unicode whitespace, non-bmp unicode" $ do
        toValidFunctionName "a_--a\66352b\6158c\65075" `shouldBe` "a_abc\65075"

generatePYSpec :: TestNames -> (PyRequest -> Text) -> Spec
generatePYSpec n gen = describe specLabel $ do
    let parseFromText = parse program ""
    it "should generate valid javascript" $ do
        let s = pyForAPI (Proxy :: Proxy TestAPI) (mconcat . map gen)
        parseFromText s `shouldSatisfy` isRight

    it "should use non-empty function names" $ do
        let (_ :<|> topLevel) = javascript (Proxy :: Proxy TopLevelRawAPI)
        output $ genJS (topLevel "GET")
        parseFromText (genJS $ topLevel "GET") `shouldSatisfy` isRight

    it "should handle simple HTTP headers" $ do
        let jsText = genJS $ javascript headerHandlingProxy
        output jsText
        parseFromText jsText `shouldSatisfy` isRight
        jsText `shouldContain` "headerFoo"
        jsText `shouldContain`  (header n "Foo" $ "headerFoo")

    it "should handle complex HTTP headers" $ do
        let jsText = genJS $ javascript customAuthProxy
        output jsText
        parseFromText jsText `shouldSatisfy` isRight
        jsText `shouldContain` "headerAuthorization"
        jsText `shouldContain`  (header n "Authorization" $ "\"Basic \" + headerAuthorization")

    it "should handle complex, custom HTTP headers" $ do
        let jsText = genJS $ javascript customHeaderProxy
        output jsText
        parseFromText jsText `shouldSatisfy` isRight
        jsText `shouldContain` "headerXThreeMilePilot"
        jsText `shouldContain`  (header n "X-ThreeMilePilot" $ "\"I am good friends with \" + headerXThreeMilePilot")

    it "should handle complex, custom HTTP headers (template replacement)" $ do
        let jsText = genJS $ javascript customHeaderProxy2
        output jsText
        parseFromText jsText `shouldSatisfy` isRight
        jsText `shouldContain` "headerXKlikatatIkatowi"
        jsText `shouldContain`  (header n "X-KlikatatIkatowi" $ "\"I would like to hear Swing Kids, Stick Figure Caraousel, \" + headerXKlikatatIkatowi + \" .\"")

    it "can generate the whole javascript code string at once with pyForAPI" $ do
        let jsStr = pyForAPI (Proxy :: Proxy TestAPI) (mconcat . map gen)
        parseFromText jsStr `shouldSatisfy` isRight
    where
        specLabel = "generateJS(" <> (show n) <> ")"
        output _ = return ()
        genJS req = gen req
        header :: TestNames -> Text -> Text -> Text
        header v headerName headerValue
            | v `elem` [Requests, RequestsWith] = "xhr.setRequestHeader(\"" <> headerName <> "\", " <> headerValue <> ");\n"
            | otherwise = "headers: { \"" <> headerName <> "\": " <> headerValue <> " }\n"
