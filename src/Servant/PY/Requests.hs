{-# LANGUAGE DataKinds             #-}
{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeOperators         #-}

module Servant.PY.Requests where

import           Control.Lens
import           Data.Maybe          (isJust)
import           Data.Monoid
import           Data.Proxy
import           Data.Text           (Text)
import qualified Data.Text           as T
import           Data.Text.Encoding  (decodeUtf8)
import           Servant.Foreign

import           Servant.PY.Internal

-- | Generate python functions that use the requests library.
--   Uses 'defCommonGeneratorOptions' for the generator options.
requests :: PythonGenerator
requests reqs = defPyImports <> mconcat (map requestsWithDef reqs)

-- | Generate python functions that use the requests library.
--   Lets you specify your own 'CommonGeneratorOptions'.
requestsWith :: CommonGeneratorOptions -> PythonGenerator
requestsWith opts reqs = mconcat (map (generatePyRequestWith opts) reqs)

-- | python codegen using requests with default options
requestsWithDef :: PyRequest -> Text
requestsWithDef = generatePyRequestWith defCommonGeneratorOptions

defPyImports :: Text
defPyImports =
  T.unlines
    [ "from urllib import parse"
    , "" -- Separate stdlib from 3rd-party imports
    , "import requests"
    ]

-- | python codegen with requests
generatePyRequestWith :: CommonGeneratorOptions -> PyRequest -> Text
generatePyRequestWith opts req = "\n" <>
    "def " <> fname <> "(" <> argsStr <> "):\n"
    <> indent' <> docStringMarker
    <> indent' <> buildDocString method req opts <> "\n"
    <> indent' <> docStringMarker
    <> indent' <> "url = " <> makePyUrl opts req (indent' <> indent') <> "\n\n"
    <> headerDef
    <> paramDef
    <> requestBuilder <> "(url" <> remaining (T.length requestBuilder + 1) <> "\n"
    <> functionReturn (returnMode opts) (indentation opts)
    <> "\n\n"
   -- where argsStr = functionArguments req
  where argsStr = T.intercalate ", " args
        args = captures req
             ++ map (view $ queryArgName . argPath) queryparams
             ++ body
             ++ map (toValidFunctionName
                    . (<>) "header"
                    . view (headerArg . argPath)
                    ) hs
        hs = req ^. reqHeaders
        fname = toValidFunctionName (functionNameBuilder opts $ req ^. reqFuncName)
        method = (T.toLower . decodeUtf8) $ req ^. reqMethod

        remaining = remainingReqCall $ PyRequestArgs (not . null $ hs) (not . null $ queryparams) hasBody
        paramDef
          | null queryparams = ""
          | otherwise = indent' <> "params = " <> toPyParams (indent' <> indent') queryparams <> "\n"
        headerDef
          | null hs = ""
          | otherwise = indent' <> "headers = " <> buildHeaderDict hs <> "\n"
        requestBuilder = indent' <> "resp = requests." <> method
        hasBody = isJust (req ^. reqBody)
        queryparams = req ^.. reqUrl.queryStr.traverse
        body = [requestBody opts | hasBody]
        indent' = indentation opts indent
        docStringMarker = "\"\"\"\n"

data PyRequestArgs = PyRequestArgs {
  hasHeaders :: Bool
  , hasParams :: Bool
  , hasData :: Bool
  } deriving (Show)

remainingReqCall :: PyRequestArgs -> Int -> Text
remainingReqCall reqArgs width
  | null argsAsList = ")"
  | length argsAsList == 1 = ",\n" <> offset <> head argsAsList <> ")\n"
  | otherwise = ",\n" <> offset <> T.intercalate (",\n" <> offset) argsAsList <> ")\n"
  where argsAsList = requestArgsToList reqArgs
        offset = mconcat $ replicate width " "

requestArgsToList :: PyRequestArgs -> [Text]
requestArgsToList reqArgs = map snd . filter fst $ zip bools strings
  where bools = [hasHeaders reqArgs, hasParams reqArgs, hasData reqArgs]
        strings = ["headers=headers", "params=params", "json=data"]


functionReturn :: ReturnStyle -> (Proxy Indent -> T.Text) -> T.Text
functionReturn DangerMode pyindenter = indent' <> "resp.raise_for_status()\n"
                                    <> indent' <> "return resp.json()"
  where indent' = pyindenter indent
functionReturn RawResponse pyindenter = indent' <> "return resp"
  where indent' = pyindenter indent
