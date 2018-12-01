{-# LANGUAGE DataKinds           #-}
{-# LANGUAGE KindSignatures      #-}
{-# LANGUAGE RankNTypes          #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Servant.PY.Internal
  ( PythonGenerator
  , ReturnStyle(..)
  , PythonRequest(..)
  , PyRequestArgs(..)
  , CommonGeneratorOptions(..)
  , defCommonGeneratorOptions
  , defaultPyIndent
  , indent
  , Indent
  , indenter
  , makePyUrl
  , makePyUrl'
  , segmentToStr
  , capturesToFormatArgs
  , toValidFunctionName
  , functionName
  , toPyHeader
  , retrieveHeaders
  , getHeaderDict
  , retrieveHeaderText
  , toPyDict
  , toPyParams
  , getParams
  , paramNames
  , captures
  , getMethod
  , hasBody
  , withFormattedCaptures
  , buildDocString
  , buildHeaderDict
  , functionArguments
  , formatBuilder
  , remainingReqCall
  -- re-exports
  , (:<|>)(..)
  , (:>)
  , defReq
  , reqHeaders
  , HasForeign(..)
  , HasForeignType(..)
  , GenerateList(..)
  , NoTypes
  , ArgType(..)
  , HeaderArg(..)
  , QueryArg(..)
  , Req(..)
  , Segment(..)
  , SegmentType(..)
  , Url(..)
  , Path
  , Arg(..)
  , FunctionName(..)
  , PathSegment(..)
  , concatCase
  , snakeCase
  , camelCase
  , ReqBody
  , JSON
  , FormUrlEncoded
  , Post
  , Get
  , Raw
  , Header
  ) where

import           Control.Lens                  hiding (List)
import qualified Data.CharSet                  as Set
import qualified Data.CharSet.Unicode.Category as Set
import           Data.Data
import           Data.Maybe                    (isJust)
import           Data.Monoid()
import           Data.Text                     (Text)
import qualified Data.Text                     as T
import           Data.Text.Encoding            (decodeUtf8)
import           GHC.TypeLits
import           Servant.Foreign


-- A 'PythonGenerator' just takes the data found in the API type
-- for each endpoint and generates Python code as Text.
-- There are `NoContent` requests and Text requests with typing information.
type PythonGenerator = [PythonRequest] -> Text
data PythonRequest = TypedPythonRequest (Req Text)
                   | UnTypedPythonRequest (Req NoContent)
                   deriving (Eq, Show)

-- We'd like to encode at the type-level that indentation
-- is some multiplication of whitespace (sorry: never tabs!)
type Indent = (" " :: Symbol)

indent :: Proxy Indent
indent = Proxy

-- The defaultPyIndent function is 4 spaces.
-- You can create a different indentation width by passing a different Int to indenter.
defaultPyIndent :: Proxy Indent -> Text
defaultPyIndent = indenter 4

-- But you can create alternatives by specializing the `indenter` function
-- to other Ints. Then, to get your indentation, pass `indent` to the created function
indenter :: Int -> Proxy Indent -> Text
indenter width space = mconcat $ width `replicate` (T.pack . symbolVal) space
{-# INLINE indenter #-}


-- Created python Functions can have different return styles
data ReturnStyle = DangerMode  -- Throw caution to the wind and return JSON
                 | RawResponse  -- Return response object itself


data PyRequestArgs = PyRequestArgs {
  hasHeaders  :: Bool
  , hasParams :: Bool
  , hasData   :: Bool
  } deriving (Show)

-- | This structure is used by specific implementations to let you
-- customize the output
data CommonGeneratorOptions = CommonGeneratorOptions
  {
    functionNameBuilder :: FunctionName -> Text
    -- ^ function generating function names
  , requestBody         :: Text
    -- ^ name used when a user want to send the request body
    -- (to let you redefine it)
  , urlPrefix           :: Text
    -- ^ a prefix we should add to the Url in the codegen
  , indentation         :: Proxy Indent -> Text
  -- ^ indentation to use for Python codeblocks. Create this function by passing an Int to indenter.
  , returnMode          :: ReturnStyle
  -- ^ whether the generated functions return the raw response or content
  }

-- | Default options.
--
-- @
-- > defCommonGeneratorOptions = CommonGeneratorOptions
-- >   { functionNameBuilder = snakeCase
-- >   , requestBody = "body"
-- >   , urlPrefix = ""
-- >   , indentation = "    "  -- 4 spaces
-- >   , returnMode = DangerMode
-- >   }
-- @
defCommonGeneratorOptions :: CommonGeneratorOptions
defCommonGeneratorOptions = CommonGeneratorOptions
  {
    functionNameBuilder = snakeCase
  , requestBody = "data"
  , urlPrefix = "http://localhost:8000"
  , indentation = defaultPyIndent
  , returnMode = DangerMode
  }

-- | Attempts to reduce the function name provided to that allowed by @'Foreign'@.
--
-- For valid Python function identifiers see the following:
-- https://docs.python.org/3.2/reference/lexical_analysis.html#identifiers
-- valid start chars: Lu, Ll, Lt, Lm, Lo, Nl, the underscore
-- valid continuation chars: valid start chars <> Mn, Mc, Nd, Pc
toValidFunctionName :: Text -> Text
toValidFunctionName t =
  case T.uncons t of
    Just (x,xs) ->
      setFirstChar x `T.cons` T.filter remainder xs
    Nothing -> "_"
  where
    setFirstChar c = if Set.member c firstLetterOK then c else '_'
    remainder c = Set.member c remainderOK
    firstLetterOK = filterBmpChars $ mconcat
                      [ Set.fromDistinctAscList "_"
                      , Set.lowercaseLetter
                      , Set.uppercaseLetter
                      , Set.titlecaseLetter
                      , Set.modifierLetter
                      , Set.otherLetter
                      , Set.letterNumber
                      ]
    remainderOK   = firstLetterOK
               <> filterBmpChars (mconcat
                    [ Set.nonSpacingMark
                    , Set.spacingCombiningMark
                    , Set.decimalNumber
                    , Set.connectorPunctuation
                    ])

functionName :: CommonGeneratorOptions -> PythonRequest -> Text
functionName opts (TypedPythonRequest req) = toValidFunctionName (functionNameBuilder opts $ req ^. reqFuncName)
functionName opts (UnTypedPythonRequest req) = toValidFunctionName (functionNameBuilder opts $ req ^. reqFuncName)

-- Identifiers can only contain codepoints in the Basic Multilingual Plane
-- that is, codepoints that can be encoded in UTF-16 without a surrogate pair (UCS-2)
-- that is, codepoints that can fit in 16-bits, up to 0xffff (65535)
filterBmpChars :: Set.CharSet -> Set.CharSet
filterBmpChars = Set.filter (< '\65536')

-- This function creates a dict where the keys are string representations of variable
-- names. This is due to the way arguments are passed into the function, and these
-- arguments named params. In other words, [("key", "key")] becomes: {"key": key}
toPyDict :: Text -> [Text] -> Text
toPyDict offset dict
  | null dict = "{}"
  | otherwise = "{" <> T.intercalate (",\n" <> offset) insides <> "}"
  where insides = combiner <$> dict
        combiner a = "\"" <> a <> "\": " <> a

-- Query params are passed into the function that makes the request, so we make
-- a python dict out of them.
getParams :: Text -> PythonRequest -> Text
getParams offset (TypedPythonRequest req) = toPyParams offset $ req ^.. reqUrl.queryStr.traverse
getParams offset (UnTypedPythonRequest req) = toPyParams offset $ req ^.. reqUrl.queryStr.traverse

toPyParams :: Text -> [QueryArg f] -> Text
toPyParams _ [] = ""
toPyParams offset qargs = toPyDict offset paramList
  where paramList = fmap (\qarg -> qarg ^. queryArgName.argName._PathSegment) qargs

-- We also need to make sure we can retrieve just the param names for function args.
paramNames :: PythonRequest -> [Text]
paramNames (TypedPythonRequest req) = map (view $ queryArgName . argPath) $ req ^.. reqUrl.queryStr.traverse
paramNames (UnTypedPythonRequest req) = map (view $ queryArgName . argPath) $ req ^.. reqUrl.queryStr.traverse

-- Request headers are also passed into the function that makes the request, so we make
-- a python dict out of them.
toPyHeader :: HeaderArg f -> Text
toPyHeader (HeaderArg n)
  = toValidFunctionName ("header" <> n ^. argName . _PathSegment)
toPyHeader (ReplaceHeaderArg n p)
  | pn `T.isPrefixOf` p = pv <> " + \"" <> rp <> "\""
  | pn `T.isSuffixOf` p = "\"" <> rp <> "\" + " <> pv
  | pn `T.isInfixOf` p  = "\"" <> T.replace pn ("\" + " <> pv <> " + \"") p
                             <> "\""
  | otherwise         = p
  where
    pv = toValidFunctionName ("header" <> n ^. argName . _PathSegment)
    pn = "{" <> n ^. argName . _PathSegment <> "}"
    rp = T.replace pn "" p

buildHeaderDict :: [HeaderArg f] -> Text
buildHeaderDict [] = ""
buildHeaderDict hs = "{" <> headers <> "}"
  where headers = T.intercalate ", " $ map headerStr hs
        headerStr h = "\"" <> h ^. headerArg . argPath <> "\": "
                           <> toPyHeader h

getHeaderDict :: PythonRequest -> Text
getHeaderDict (TypedPythonRequest req) = buildHeaderDict $ req ^. reqHeaders
getHeaderDict (UnTypedPythonRequest req) = buildHeaderDict $ req ^. reqHeaders

retrieveHeaders :: PythonRequest -> [Text]
retrieveHeaders (TypedPythonRequest req) = retrieveHeaderText <$> req ^. reqHeaders
retrieveHeaders (UnTypedPythonRequest req) = retrieveHeaderText <$> req ^. reqHeaders

retrieveHeaderText :: forall f. HeaderArg f -> Text
retrieveHeaderText h = h ^. headerArg . argPath


functionArguments :: forall f. Req f -> Text
functionArguments req =
  mconcat [ T.intercalate ", " args]
  where
    args = captures' req ++ qparam ++ body ++ headers

    qparam = map ((<>) "param_" . view (queryArgName . argPath)) queryParams

    body = if isJust $ req ^. reqBody
           then ["data"]
           else []
    queryParams = req ^.. reqUrl . queryStr . traverse
    headers = map ((<>) "header_"
                    . view (headerArg . argPath)
                  ) $ req ^. reqHeaders

captures :: PythonRequest -> [Text]
captures (TypedPythonRequest req)   = captures' req
captures (UnTypedPythonRequest req) = captures' req


captures' :: forall f. Req f -> [Text]
captures' req = map (view argPath . captureArg)
         . filter isCapture
         $ req ^. reqUrl.path

makePyUrl :: CommonGeneratorOptions -> PythonRequest -> Text -> Text
makePyUrl opts (TypedPythonRequest req) offset   = makePyUrl' opts req offset
makePyUrl opts (UnTypedPythonRequest req) offset = makePyUrl' opts req offset

makePyUrl' :: forall f. CommonGeneratorOptions -> Req f -> Text -> Text
makePyUrl' opts req offset = "\"" <> url <> "\""
  where url = urlPrefix opts <> "/" <> getSegments pathParts
                             <> withFormattedCaptures offset pathParts
        pathParts = req ^.. reqUrl.path.traverse

getSegments :: forall f. [Segment f] -> Text
getSegments segments = if null segments
                       then ""
                       else T.intercalate "/" (map segmentToStr segments) <> "\""

withFormattedCaptures :: Text -> [Segment f] -> Text
withFormattedCaptures offset segments = formattedCaptures (capturesToFormatArgs segments)
  where formattedCaptures [] = ""
        formattedCaptures xs = ".format(\n" <> offset
                              <> T.intercalate (",\n" <> offset) (map formatBuilder xs)
                              <> ")"

formatBuilder :: Text -> Text
formatBuilder val = val <> "=parse.quote(str("<> val <> "))"

segmentToStr :: Segment f -> Text
segmentToStr (Segment (Static s)) = s ^. _PathSegment
segmentToStr (Segment (Cap s))    = "{" <> s ^. argName . _PathSegment <> "}"

capturesToFormatArgs :: [Segment f] -> [Text]
capturesToFormatArgs segments = map getSegment $ filter isCapture segments
  where getSegment (Segment (Cap a)) = getCapture a
        getSegment _                 = ""
        getCapture s = s ^. argName . _PathSegment

captureArgsWithTypes :: [Segment Text] -> [Text]
captureArgsWithTypes segments =  map getSegmentArgType (filter isCapture segments)
  where getSegmentArgType (Segment (Cap a)) = pathPart a <> " (" <> a ^. argType <> ")"
        getSegmentArgType _                 = ""
        pathPart s = s ^. argName . _PathSegment


buildDocString :: PythonRequest -> CommonGeneratorOptions -> Text -> Text
buildDocString (TypedPythonRequest req) opts returnVal = buildDocString' req opts args returnVal
  where args = captureArgsWithTypes $ req ^.. reqUrl.path.traverse
buildDocString (UnTypedPythonRequest req) opts returnVal = buildDocString' req opts args returnVal
  where args = capturesToFormatArgs $ req ^.. reqUrl.path.traverse

buildDocString' :: forall f. Req f -> CommonGeneratorOptions -> [Text] -> Text -> Text
buildDocString' req opts args returnVal = T.toUpper method <> " \"" <> url <> "\n"
                                                  <> includeArgs <> "\n\n"
                                                  <> indent' <> "Returns:\n"
                                                  <> indent' <> indent' <> returnVal
  where method = decodeUtf8 $ req ^. reqMethod
        url = getSegments $ req ^.. reqUrl.path.traverse
        includeArgs = if null args then "" else argDocs
        argDocs = indent' <> "Args:\n"
                  <> indent' <> indent' <> T.intercalate ("\n" <> indent' <> indent') args
        indent' = indentation opts indent

getMethod :: PythonRequest -> Text
getMethod (TypedPythonRequest req) = decodeUtf8 $ req ^. reqMethod
getMethod (UnTypedPythonRequest req) = decodeUtf8 $ req ^. reqMethod

hasBody :: PythonRequest -> Bool
hasBody (TypedPythonRequest req) = isJust (req ^. reqBody)
hasBody (UnTypedPythonRequest req) = isJust (req ^. reqBody)

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
