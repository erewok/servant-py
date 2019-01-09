module Servant.PY.Treq
  ( treq
  ) where

import           Data.Monoid
  ( (<>)
  )
import           Data.Proxy
   (Proxy
   )
import           Data.Text
  ( Text
  )

import qualified Data.Text           as T

import           Servant.PY.Internal
  ( PythonRequest
  , PythonGenerator
  , CommonGeneratorOptions(returnMode, indentation, requestBody)
  , ReturnStyle(DangerMode, RawResponse)
  , Indent
  , PyRequestArgs(PyRequestArgs)
  , defCommonGeneratorOptions
  , functionName
  , buildDocString
  , makePyUrl
  , indent
  , hasBody
  , getHeaderDict
  , getParams
  , remainingReqCall
  , getMethod
  , paramNames
  , retrieveHeaders
  , toValidFunctionName
  , captures
  )

-- | Generate python functions that use the treq library.
--   Uses 'defCommonGeneratorOptions' for the generator options.
treq :: PythonGenerator
treq reqs = defPyImports <> mconcat (map treqWithDef reqs)

defPyImports :: Text
defPyImports =
  T.unlines
    [ "try: from urllib import parse" -- Python 3
    , "except: import urllib as parse" -- Python 2
    , ""
    , "from twisted.internet.defer import inlineCallbacks, returnValue"
    ]

treqWithDef :: PythonRequest -> Text
treqWithDef = generatePyTreqWith defCommonGeneratorOptions

generatePyTreqWith :: CommonGeneratorOptions -> PythonRequest -> Text
generatePyTreqWith opts req =
  "\n"
  <> "@inlineCallbacks\n"
  <> "def " <> functionName opts req <> "(" <> argsStr <> "):\n"
  <> indent' <> docStringMarker
  <> indent' <> buildDocString req opts returnVal <> "\n"
  <> indent' <> docStringMarker
  <> indent' <> "url = " <> makePyUrl opts req (indent' <> indent') <> "\n\n"
  <> headerDef
  <> paramDef
  <> requestBuilder <> "(url" <> remaining (T.length requestBuilder + 1) <> "\n"
  <> functionReturn (returnMode opts) (indentation opts)
  <> "\n\n"
  where argsStr = T.intercalate ", " args
        args = [ "treq" ]
             ++ captures req
             ++ qparams
             ++ body
             ++ map (toValidFunctionName
                    . (<>) "header"
                    ) hs
        hs = retrieveHeaders req
        qparams = paramNames req
        method = T.toLower $ getMethod req
        remaining = remainingReqCall $ PyRequestArgs (not . null $ hs) (not . null $ qparams) (hasBody req)
        paramDef
          | null qparams = ""
          | otherwise = indent' <> "params = " <> getParams (indent' <> indent') req <> "\n"
        headerDef
          | null hs = ""
          | otherwise = indent' <> "headers = " <> getHeaderDict req <> "\n"
        requestBuilder = indent' <> "resp = yield treq." <> method
        body = [requestBody opts | hasBody req]
        indent' = indentation opts indent
        docStringMarker = "\"\"\"\n"
        returnVal = case returnMode opts of
          DangerMode -> "JSON response from the endpoint"
          RawResponse -> "response (IResponse) from issuing the request"

functionReturn :: ReturnStyle -> (Proxy Indent -> T.Text) -> T.Text
functionReturn DangerMode pyindenter =
  indent' <> "if resp.code < 200 or resp.code > 299:\n"
  <> indent' <> indent' <> "raise Exception(resp)\n"
  <> indent' <> "returnValue((yield resp.json_content()))\n"
  where indent' = pyindenter indent

functionReturn RawResponse pyindenter = indent' <> "returnValue(resp)"
  where indent' = pyindenter indent
