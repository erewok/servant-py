module Servant.PY.Requests where

import           Data.Monoid
import           Data.Proxy
import           Data.Text           (Text)
import qualified Data.Text           as T

import           Servant.PY.Internal

-- | Generate python functions that use the requests library.
--   Uses 'defCommonGeneratorOptions' for the generator options.
requests :: PythonGenerator
requests reqs = defPyImports <> mconcat (map requestsWithDef reqs)

-- | Generate python functions that use the requests library.
--   Lets you specify your own 'CommonGeneratorOptions'.
requestsWith :: CommonGeneratorOptions -> [PythonRequest] -> Text
requestsWith opts reqs = mconcat (map (generatePyRequestWith opts) reqs)

-- | python codegen using requests with default options
requestsWithDef :: PythonRequest -> Text
requestsWithDef = generatePyRequestWith defCommonGeneratorOptions

defPyImports :: Text
defPyImports =
  T.unlines
    [ "from urllib import parse"
    , "" -- Separate stdlib from 3rd-party imports
    , "import requests"
    ]

-- | python codegen with requests
generatePyRequestWith :: CommonGeneratorOptions -> PythonRequest -> Text
generatePyRequestWith opts req = "\n" <>
    "def " <> functionName opts req <> "(" <> argsStr <> "):\n"
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
        args = captures req
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
        requestBuilder = indent' <> "resp = requests." <> method
        body = [requestBody opts | hasBody req]
        indent' = indentation opts indent
        docStringMarker = "\"\"\"\n"
        returnVal = case returnMode opts of
          DangerMode -> "JSON response from the endpoint"
          RawResponse -> "response (requests.Response) from issuing the request"

functionReturn :: ReturnStyle -> (Proxy Indent -> T.Text) -> T.Text
functionReturn DangerMode pyindenter = indent' <> "resp.raise_for_status()\n"
                                    <> indent' <> "return resp.json()"
  where indent' = pyindenter indent
functionReturn RawResponse pyindenter = indent' <> "return resp"
  where indent' = pyindenter indent
