{-# LANGUAGE OverloadedStrings #-}

module Servant.PY.TreqSpec
  ( spec
  ) where

import Data.Proxy
  ( Proxy(Proxy)
  )

import Servant.API
  ( NoContent
  )

import Servant.Foreign
  ( listFromAPI
  )

import Servant.PY
  ( NoTypes
  )

import Servant.PY.Internal
  ( PythonRequest(UnTypedPythonRequest)
  )

import Servant.PY.Treq
  ( treq
  )

import Servant.PY.InternalSpec
  ( testApi
  , captureApi
  , shouldContain
  )

import Test.Hspec
  ( Spec
  , describe
  , it
  )

spec :: Spec
spec = describe "Servant.PY.Treq" treqSpec

treqSpec :: Spec
treqSpec = describe "Treq" $ do
  describe "treq" $ do
    describe "without captures" $
      let
        reqs = listFromAPI (Proxy :: Proxy NoTypes) (Proxy :: Proxy NoContent) testApi
        pyReqs = map UnTypedPythonRequest reqs
        source = treq pyReqs
      in do
        it "Must import this infrastructure stuff" $
          source `shouldContain` "\nfrom twisted.internet.defer import inlineCallbacks, returnValue\n"
        it "A function signature should look about like this" $
          source `shouldContain` "\n@inlineCallbacks\ndef post_counterreqheader(treq):\n"
        it "The actual request is basically like this" $
          source `shouldContain` "\n    resp = yield treq.post(url)\n"
        it "If there are headers, we should pass them." $ do
          source `shouldContain` "headers = {\"Some-Header\": headerSomeHeader}"
          source `shouldContain` " headers=headers"
        it "If there are params, we should pass them." $ do
          source `shouldContain` "params = {\"sortby\": sortby}"
          source `shouldContain` "params=params"
    describe "with captures" $
      let
        reqs = listFromAPI (Proxy :: Proxy NoTypes) (Proxy :: Proxy NoContent) captureApi
        pyReqs = map UnTypedPythonRequest reqs
        source = treq pyReqs
      in do
        it "A function signature needs parameters for the captures." $
          source `shouldContain` "\n@inlineCallbacks\ndef post_loginwithpathvarandheader_by_id_by_Name_by_hungrig(treq, id, Name, hungrig, data):\n"
        it "Captures should be quoted into the url" $
          source `shouldContain` "id=parse.quote(str(id)),"
        it "Payload should be passed along as well." $
          source `shouldContain` "json=data"
