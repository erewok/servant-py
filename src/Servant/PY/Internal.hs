{-# LANGUAGE DataKinds         #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE KindSignatures    #-}
{-# LANGUAGE RankNTypes        #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Servant.PY.Internal
  ( PythonGenerator
  , ReturnStyle(..)
  , InformationLevel(..)
  , CommonGeneratorOptions(..)
  , defCommonGeneratorOptions
  , PyRequest
  , defaultPyIndent
  , indent
  , Indent
  , indenter
  , makePyUrl
  , makePyUrl'
  , segmentToStr
  , capturesToFormatArgs
  , toValidFunctionName
  , toPyHeader
  , toPyDict
  , toPyParams
  , captures
  , withFormattedCaptures
  , buildDocString
  , buildHeaderDict
  , functionArguments
  , formatBuilder
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
import           Data.Monoid
import           Data.Text                     (Text)
import qualified Data.Text                     as T
import           Data.Text.Encoding            (decodeUtf8)
import           GHC.TypeLits
import           Servant.Foreign


-- A 'PythonGenerator' just takes the data found in the API type
-- for each endpoint and generates Python code as Text.
type PythonGenerator = [PyRequest] -> Text
type PyRequest = Req NoContent

-- We'd like to encode at the type-level that indentation
-- is some multiplication of whitespace (sorry: never tabs!)
type Indent = (" " :: Symbol)

indent :: Proxy Indent
indent = Proxy

-- The defaultPyIndent function is 4 spaces.
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


data InformationLevel = AsMuchAsPossible  -- Must use DeriveDataTypeable and do deriving (Data, Typeable)
                      | Minimal  -- Really doesn't say much abotu the arguments of functions or return vals

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
  -- ^ indentation to use for Python codeblocks
  , returnMode          :: ReturnStyle
  -- ^ whether the generated functions return the raw response or content
  , informationMode     :: InformationLevel
  -- ^ if we should include more information on request bodies and objects returned
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
-- >   , informationMode = Minimal
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
  , informationMode = Minimal
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

-- Javascript identifiers can only contain codepoints in the Basic Multilingual Plane
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
toPyParams :: Text -> [QueryArg f] -> Text
toPyParams _ [] = ""
toPyParams offset qargs = toPyDict offset paramList
  where paramList = fmap (\qarg -> qarg ^. queryArgName.argName._PathSegment) qargs

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

captures :: Req NoContent -> [T.Text]
captures req = map (view argPath . captureArg)
         . filter isCapture
         $ req ^. reqUrl.path

buildHeaderDict :: [HeaderArg f] -> Text
buildHeaderDict [] = ""
buildHeaderDict hs = "{" <> headers <> "}"
  where headers = T.intercalate ", " $ map headerStr hs
        headerStr header = "\"" <> header ^. headerArg . argPath <> "\": "
                           <> toPyHeader header

functionArguments :: PyRequest -> T.Text
functionArguments req =
  mconcat [ T.intercalate ", " args]
  where
    args = captures' ++ qparam ++ body ++ headers

    captures' = map (view argPath . captureArg)
               $ filter isCapture
               $ req ^. reqUrl . path

    qparam = map ((<>) "param_" . view (queryArgName . argPath)) queryParams

    body = if isJust $ req ^. reqBody
           then ["data"]
           else []
    queryParams = req ^.. reqUrl . queryStr . traverse
    headers = map ((<>) "header_"
                    . view (headerArg . argPath)
                  ) $ req ^. reqHeaders


makePyUrl :: forall f. CommonGeneratorOptions -> Req f -> Text -> Text
makePyUrl opts req offset = if url' == "\"" then "\"/\"" else url'
  where url' = "\"" <> urlPrefix opts <> "/"
                    <> makePyUrl' pathParts
                    <> withFormattedCaptures offset pathParts
        pathParts = req ^.. reqUrl.path.traverse

makePyUrl' :: [Segment f] -> Text
makePyUrl' []       = ""
makePyUrl' segments = T.intercalate "/" (map segmentToStr segments) <> "\""

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

buildDocString :: PyRequest -> CommonGeneratorOptions -> T.Text
buildDocString req opts = T.toUpper method <> " \"" <> url <> "\n"
                                                  <> includeArgs <> "\n\n"
                                                  <> indent' <> "Returns: " <> "\n"
                                                  <> indent' <> indent' <> returnVal
  where args = capturesToFormatArgs $ req ^.. reqUrl.path.traverse
        method = decodeUtf8 $ req ^. reqMethod
        url = makePyUrl' $ req ^.. reqUrl.path.traverse
        includeArgs = if null args then "" else argDocs
        argDocs = indent' <> "Args: " <> "\n"
                  <> indent' <> indent' <> T.intercalate ("\n" <> indent' <> indent') args
        indent' = indentation opts indent
        returnVal = case returnMode opts of
          DangerMode -> "JSON response from the endpoint"
          RawResponse -> "response (requests.Response) from issuing the request"
