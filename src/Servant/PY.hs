{-# LANGUAGE DataKinds             #-}
{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeOperators         #-}

module Servant.PY ( -- * Generating javascript code from an API type
    pyForAPI
  , writePythonForAPI
  , PythonGenerator

  , -- * Options common to all generators
    CommonGeneratorOptions(..)
  , defCommonGeneratorOptions

  -- Requests library
  , requests


  , -- * Function renamers
    concatCase
  , snakeCase
  , camelCase

  , -- * Misc.
    listFromAPI
  , python
  , NoTypes
  , GenerateList(..)
  , FunctionName(..)
  ) where

import Data.Proxy
import           Data.Text           (Text)
import qualified Data.Text           as T
import           Servant.Foreign

import           Servant.PY.Internal
import           Servant.PY.Requests

-- | Generate the data necessary to generate Python code
--   for all the endpoints of an API, as ':<|>'-separated values
--   of type 'PyRequest'.
python :: HasForeign NoTypes NoContent api => Proxy api -> Foreign NoContent api
python p = foreignFor (Proxy :: Proxy NoTypes) (Proxy :: Proxy NoContent) p defReq

-- | Directly generate all the Python functions for your API
--   from a 'Proxy' for your API type. You can then write it to
--   a file or integrate it in a page, for example.
pyForAPI :: (HasForeign NoTypes NoContent api, GenerateList NoContent (Foreign NoContent api))
         => Proxy api -- ^ proxy for your API type
         -> PythonGenerator -- ^ python code generator to use (requests is the only one for now)
         -> Text                -- ^ a text that you can embed in your pages or write to a file
pyForAPI p gen = gen (listFromAPI (Proxy :: Proxy NoTypes) (Proxy :: Proxy NoContent) p)

-- | Directly generate all the Python functions for your API
--   from a 'Proxy' for your API type using the given generator
--   and write the resulting code to a file at the given path.
writePythonForAPI :: (HasForeign NoTypes NoContent api, GenerateList NoContent (Foreign NoContent api))
              => Proxy api -- ^ proxy for your API type
              -> PythonGenerator -- ^ python code generator to use (requests is the only one for now)
              -> FilePath -- ^ path to the file you want to write the resulting javascript code into
              -> IO ()
writePythonForAPI p gen fp = writeFile fp (T.unpack $ pyForAPI p gen)
