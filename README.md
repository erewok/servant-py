# servant-py

This library lets you derive automatically Python functions that let you query each endpoint of a *servant* webservice.

Currently, the only supported method for generating requests is via the `requests` library, which is the recommended way to generate HTTP requests in the Python world (even among Python core devs).

## Inspiration

This library is largely inspired by [servant-js](https://github.com/haskell-servant/servant-js) and by the fantastic work of the Servant team in general. Any good ideas you find in here are from their work (any mistakes are almost entirely mine, however).

## Example

There are two different styles of function-return supported here: `DangerMode` and `RawResponse`.

The latter returns the raw response from issuing the request and the former calls `raise_for_status` and then attempts to return `resp.json()`. You can switch which style you'd like to use by creating a proper `CommonGeneratorOptions` object.

The default options just chucks it all to the wind and goes for `DangerMode` (because, seriously, we're using Haskell to generate Python here...).

Following is an example of using the Servant DSL to describe endpoints and then using `servant-py` to create Python clients for those endpoints.

#### Servant DSL API Description

``` haskell
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
```

#### Generated Python Code

If you build the above and run it, you will get some output that looks like the following:

```python

from urllib import parse

import requests

def post_counterreqheader():
    """
    POST "counter-req-header"

    """
    url = "/counter-req-header"

    resp = requests.post(url)
    resp.raise_for_status()
    return resp.json()


def get_counterqueryparam(sortby, header_SomeHeader):
    """
    GET "counter-queryparam"

    """
    url = "/counter-queryparam"

    headers = {"Some-Header": headerSomeHeader}
    params = {"sortby": sortby}
    resp = requests.get(url
                        headers=headers,
                        params=params)

    resp.raise_for_status()
    return resp.json()


def get_loginqueryflag(published):
    """
    GET "login-queryflag"

    """
    url = "/login-queryflag"

    params = {"published": published}
    resp = requests.get(url
                        params=params)

    resp.raise_for_status()
    return resp.json()


def post_loginparamsauthorswithreqBody(authors, data):
    """
    POST "login-params-authors-with-reqBody"

    """
    url = "/login-params-authors-with-reqBody"

    params = {"authors": authors}
    resp = requests.post(url
                         params=params,
                         json=data)

    resp.raise_for_status()
    return resp.json()


def post_loginwithpathvarandheader_by_id_by_Name_by_hungrig(id, Name, hungrig, data):
    """
    POST "login-with-path-var-and-header/{id}/{Name}/{hungrig}"
    Args:
        id
        Name
        hungrig
    """
    url = "/login-with-path-var-and-header/{id}/{Name}/{hungrig}".format(
        id=parse.quote(id),
        Name=parse.quote(Name),
        hungrig=parse.quote(hungrig))

    resp = requests.post(url
                         json=data)

    resp.raise_for_status()
    return resp.json()
```

If you would like to compile and run this example yourself, you can do that like so:

```
$ stack build --flag servant-py:example
$ stack exec servant-py-exe
$ cat examples/api.py
```

## TODO

1. Add Tests Pronto!
2. Fix `urllib.parse.quote` on non-string args. Need to know more about what's getting passed or convert all to strings.
