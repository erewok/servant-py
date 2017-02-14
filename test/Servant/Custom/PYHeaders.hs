{-# LANGUAGE DataKinds           #-}
{-# LANGUAGE FlexibleContexts    #-}
{-# LANGUAGE FlexibleInstances   #-}
{-# LANGUAGE KindSignatures      #-}
{-# LANGUAGE PolyKinds           #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeFamilies        #-}
{-# LANGUAGE TypeOperators       #-}
{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE MultiParamTypeClasses #-}

module Servant.Custom.PYHeaders where

import           Control.Lens
import           Data.Monoid
import           Data.Proxy
import           Data.Text (pack)
import           GHC.TypeLits
import           Servant.API.ContentTypes
import           Servant.PY.Internal

-- | This is a hypothetical combinator that fetches an Authorization header.
-- The symbol in the header denotes what kind of authentication we are
-- using -- Basic, Digest, whatever.
data Authorization (sym :: Symbol) a

instance (KnownSymbol sym, HasForeign lang NoContent api)
    => HasForeign lang NoContent (Authorization sym a :> api) where
    type Foreign NoContent (Authorization sym a :> api) = Foreign NoContent api

    foreignFor lang ftype Proxy req = foreignFor lang ftype (Proxy :: Proxy api) $
        req & reqHeaders <>~
          [ ReplaceHeaderArg (Arg "Authorization" NoContent)
          $ tokenType (pack . symbolVal $ (Proxy :: Proxy sym)) ]
      where
        tokenType t = t <> " {Authorization}"

-- | This is a combinator that fetches an X-ThreeMilePilot header.
data ThreeMilePilot a

instance (HasForeign lang NoContent api)
    => HasForeign lang NoContent (ThreeMilePilot a :> api) where
    type Foreign NoContent (ThreeMilePilot a :> api) = Foreign NoContent api

    foreignFor lang ftype Proxy req = foreignFor lang ftype (Proxy :: Proxy api) $
        req & reqHeaders <>~ [ ReplaceHeaderArg (Arg "X-ThreeMilePilot" NoContent) tpl ]
      where
        tpl = "I am good friends with {X-ThreeMilePilot}"

-- | This is a combinator that fetches an X-WhatsForDinner header.
data KlikatatIkatowi a

instance (HasForeign lang NoContent api)
    => HasForeign lang NoContent (KlikatatIkatowi a :> api) where
    type Foreign NoContent (KlikatatIkatowi a :> api) = Foreign NoContent api

    foreignFor lang ftype Proxy req = foreignFor lang ftype (Proxy :: Proxy api) $
        req & reqHeaders <>~ [ ReplaceHeaderArg (Arg "X-KlikatatIkatowi" NoContent) tpl ]
      where
        tpl = "I would like to hear Swing Kids, Stick Figure Caraousel, {X-KlikatatIkatowi}."
