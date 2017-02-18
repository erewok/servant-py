{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE ScopedTypeVariables   #-}

module Servant.PY.Python where

-- import Data.Aeson
import Data.Data
import qualified Data.ByteString.Char8 as B
import qualified Data.ByteString.Lazy as LB
import           Data.Monoid           ((<>))
import           Data.Proxy
import Data.Text (Text)
import qualified Data.Text as T
import           Servant.Foreign

data Python

getFieldsFor :: forall a. Data a => a -> [Text]
getFieldsFor = map T.pack . mconcat . map constrFields . getConstr
  where getConstr = dataTypeConstrs . dataTypeOf

getFieldTypesFor :: forall a. Typeable a => a -> [Text]
getFieldTypesFor = init . T.splitOn " -> " . T.pack . show . typeOf

instance (Typeable a, Data a) => HasForeignType Python Text a where
  typeFor _ _ d = "{ lba" <> T.intercalate ", " (map printPair fieldPairs) <> "}"
    where printPair (a, b) = "\"" <> a <> "\": " <> b
          fieldPairs = zip fieldNames fieldTypes
          fieldNames = getFieldsFor d
          fieldTypes = getFieldTypesFor d


-- instance HasForeignType Python Text NoContent where
--   typeFor _ _ _ = "None"
--
-- instance HasForeignType Python Text Int where
--   typeFor _ _ _ = "int"

-- instance HasForeignType Python Text Bool where
--   typeFor _ _ _ = "bool"
--
-- instance HasForeignType Python Text String where
--   typeFor _ _ _ = "str"

-- instance HasForeignType Python Text Text where
--   typeFor _ _ _ = "str"
--
-- instance HasForeignType Python Text LB.ByteString where
--   typeFor _ _ _ = "str"
--
-- instance HasForeignType Python Text B.ByteString where
--   typeFor _ _ _ = "str"
--
-- instance HasForeignType Python Text JSON where
--   typeFor _ _ _ = "dict"

instance HasForeignType Python Text a => HasForeignType Python Text (Header a) where
  typeFor lang ftype _ = "dict" <> typeFor lang ftype (Proxy :: Proxy a)

instance HasForeignType Python Text a => HasForeignType Python Text (Headers '[Header a b] c) where
  typeFor lang ftype _ = "dict" <> typeFor lang ftype (Proxy :: Proxy a)

instance HasForeignType Python Text a => HasForeignType Python Text [Header a b] where
  typeFor lang ftype _ = "[dict of " <> typeFor lang ftype (Proxy :: Proxy a) <> "]"

instance HasForeignType Python Text a => HasForeignType Python Text (Headers a) where
  typeFor lang ftype _ = "dict" <> typeFor lang ftype (Proxy :: Proxy a)
--
-- instance HasForeignType Python Text a => HasForeignType Python Text (Maybe a) where
--   typeFor lang ftype _ = "Optional" <> typeFor lang ftype (Proxy :: Proxy a)
--
-- instance HasForeignType Python Text a => HasForeignType Python Text [a] where
--    typeFor lang ftype _ = "list of " <> typeFor lang ftype (Proxy :: Proxy a)
