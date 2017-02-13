{-# LANGUAGE DataKinds                  #-}
{-# LANGUAGE DeriveGeneric              #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE TypeOperators              #-}

module Main where

import           Data.Aeson
import qualified Data.ByteString.Char8 as B
import           Data.Proxy
import qualified Data.Text             as T
import           GHC.Generics
import           Servant
import           System.FilePath

import           Servant.PY

-- * A simple Counter data type
newtype Counter = Counter { value :: Int }
  deriving (Generic, Show, Num)
instance ToJSON Counter

data LoginForm = LoginForm
 { username :: !T.Text
 , password :: !T.Text
 , otherMissing :: Maybe T.Text
 } deriving (Eq, Show, Generic)
instance ToJSON LoginForm

-- * Our API type
type TestApi = "counter-req-header" :> Post '[JSON] Counter
          :<|> "counter-queryparam"
            :> QueryParam "sortby" T.Text
            :> Header "Some-Header" T.Text :> Get '[JSON] Counter
          :<|> "login-queryflag" :> QueryFlag "published" :> Get '[JSON] LoginForm
          :<|> "login-params-authors-with-reqBody"
            :> QueryParams "authors" T.Text
            :> ReqBody '[JSON] LoginForm :> Post '[JSON] LoginForm
          :<|> "login-with-path-var-and-header"
            :> Capture "id" Int
            :> Capture "Name" T.Text
            :> Capture "hungrig" Bool
            :> ReqBody '[JSON] LoginForm
            :> Post '[JSON] (Headers '[Header "test-head" B.ByteString] LoginForm)

testApi :: Proxy TestApi
testApi = Proxy

-- where our static files reside
result :: FilePath
result = "examples"

main :: IO ()
main = writePythonForAPI testApi requests (result </> "api.py")
