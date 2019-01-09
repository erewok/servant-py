{-# LANGUAGE DataKinds             #-}
{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeOperators         #-}

module Servant.PY ( -- * Generating python code from an API type
  PythonGenerator
  , python
  , pythonTyped
  , writePythonForAPI
  , pyForAPI
  , pyTypedForAPI
  , writeTypedPythonForAPI
  , -- * Options common to all generators
    CommonGeneratorOptions(..)
  , defCommonGeneratorOptions

  -- Requests library
  , requests
  -- Treq library
  , treq

  , -- * Function renamers
    concatCase
  , snakeCase
  , camelCase

  , -- * Misc.
    listFromAPI
  , NoTypes
  , GenerateList(..)
  , FunctionName(..)
  ) where

import           Data.Proxy
import           Data.Text           (Text)
import qualified Data.Text           as T
import           Servant.Foreign

import           Servant.PY.Internal
import           Servant.PY.Python
import           Servant.PY.Requests
import           Servant.PY.Treq     (treq)
-- | Generate the data necessary to generate Python code
--   for all the endpoints of an API, as ':<|>'-separated values
--   of type 'PyRequest'.
python :: HasForeign NoTypes NoContent api => Proxy api -> Foreign NoContent api
python p = foreignFor (Proxy :: Proxy NoTypes) (Proxy :: Proxy NoContent) p defReq

-- | Generate the data necessary to generate Python code
--   for all the endpoints of an API, but try to get as much type-information
-- into Python docstrings, in order to aid discoverability of client functions.
pythonTyped :: HasForeign Python Text api => Proxy api -> Foreign Text api
pythonTyped p = foreignFor (Proxy :: Proxy Python) (Proxy :: Proxy Text) p defReq

-- | Directly generate all the Python functions for your API
--   from a 'Proxy' for your API type. You can then write it to
--   a file or integrate it in a page, for example.
pyForAPI :: (HasForeign NoTypes NoContent api, GenerateList NoContent (Foreign NoContent api))
         => Proxy api -- ^ proxy for your API type
         -> PythonGenerator -- ^ python code generator to use (requests or treq)
         -> Text                -- ^ a text that you can embed in your pages or write to a file
pyForAPI p gen = gen (UnTypedPythonRequest <$> listFromAPI (Proxy :: Proxy NoTypes) (Proxy :: Proxy NoContent) p)

-- | Directly generate all the Python functions for your API
--   from a 'Proxy' for your API type using the given generator
--   and write the resulting code to a file at the given path.
writePythonForAPI :: (HasForeign NoTypes NoContent api, GenerateList NoContent (Foreign NoContent api))
              => Proxy api -- ^ proxy for your API type
              -> PythonGenerator -- ^ python code generator to use (requests or treq)
              -> FilePath -- ^ path to the file you want to write the resulting javascript code into
              -> IO ()
writePythonForAPI p gen fp = writeFile fp (T.unpack $ pyForAPI p gen)


-- | Directly generate all the Python functions for your API
--   from a 'Proxy' for your API type. You can then write it to
--   a file or integrate it in a page, for example.
pyTypedForAPI :: (HasForeign Python T.Text api, GenerateList T.Text (Foreign T.Text api))
         => Proxy api -- ^ proxy for your API type
         -> PythonGenerator -- ^ python code generator to use (requests or treq)
         -> Text                -- ^ a text that you can embed in your pages or write to a file
pyTypedForAPI p gen = gen (TypedPythonRequest <$> listFromAPI (Proxy :: Proxy Python) (Proxy :: Proxy T.Text) p)


writeTypedPythonForAPI :: (HasForeign Python T.Text api, GenerateList T.Text (Foreign T.Text api))
              => Proxy api -- ^ proxy for your API type
              -> PythonGenerator -- ^ python code generator to use (requests or treq)
              -> FilePath -- ^ path to the file you want to write the resulting javascript code into
              -> IO ()
writeTypedPythonForAPI p gen fp = writeFile fp (T.unpack $ pyTypedForAPI p gen)
