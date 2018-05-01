{-# LANGUAGE DataKinds, TypeOperators, DuplicateRecordFields, DeriveGeneric, GeneralizedNewtypeDeriving, OverloadedStrings #-}
module EBCR where

import Servant.API
import Data.Map
import Data.Proxy
import GHC.Generics
import Data.Aeson (FromJSON(..), ToJSON(..), FromJSONKey, ToJSONKey, (.=), object)
import Web.HttpApiData
import Data.Typeable

newtype Feature = Feature String deriving (Typeable, Generic, Show, Ord, Eq, FromJSONKey, ToJSONKey)
newtype Value = Value String deriving (Generic, Typeable, Show)
type Features = Map Feature Value

data Op = GT | LT | GE | LE | EQ | NE deriving (Generic, Typeable, Show)

data FeatureInequality = FeatureInequality Feature Op Value deriving (Generic, Typeable, Show)

newtype CurieIdentifier = CurieIdentifier String deriving (Generic, Typeable, Show, FromHttpApiData, ToHttpApiData)

data Bounds = Bounds {
    lower_bound :: Int,
    upper_bound :: Int
} deriving (Generic, Typeable, Show)

data Cohort = Cohort {
    cohort_id :: CurieIdentifier,
    cohort_size :: Bounds
} | Error {
    error_message :: String
} deriving (Generic, Typeable, Show)

data Features2 = Features2 {
    cohort_id :: CurieIdentifier,
    feature_a :: FeatureInequality,
    feature_b :: FeatureInequality
} deriving (Generic, Typeable, Show)

data Feature1 = Feature1 {
    cohort_id :: CurieIdentifier,
    feature :: FeatureInequality,
    maximum_p_value :: Double
} deriving (Generic, Typeable, Show)

data FeaturesAssociation = FeaturesAssociation {
    chi_squared :: Double,
    p_value :: Double,
    feature_matrix :: ((Int,Int),(Int, Int))
} deriving (Generic, Typeable, Show)

data AssociationToAllFeatures = AssociationToAllFeatures {
    corrected_chi_squared_statistics :: [Double],
    p_values :: [Double],
    feature_matrix :: [(Feature,Int,Int,Double,Double)]
} deriving (Generic, Typeable, Show)

instance ToJSON Feature
instance FromJSON Feature
instance ToJSON Value
instance FromJSON Value
instance ToJSON CurieIdentifier
instance FromJSON CurieIdentifier
instance ToJSON Op
instance FromJSON Op
instance ToJSON FeatureInequality
instance FromJSON FeatureInequality
instance ToJSON Bounds
instance FromJSON Bounds
instance ToJSON Cohort where
    toJSON (Cohort id size) = object ["cohort_id" .= id, "cohort_size" .= size]
    toJSON (Error message) = toJSON message
-- instance FromJSON Cohort where

instance ToJSON Features2
instance FromJSON Features2
instance ToJSON Feature1
instance FromJSON Feature1
instance ToJSON FeaturesAssociation
instance FromJSON FeaturesAssociation
instance ToJSON AssociationToAllFeatures
instance FromJSON AssociationToAllFeatures

type EBCRAPI_get_ids_by_feature = "get_ids_by_feature" :> ReqBody' '[Required, Strict] '[JSON] Features :> Get '[JSON] Cohort
type EBCRAPI_get_features_by_id = "get_features_by_id" :> QueryParam' '[Required, Strict] "cohort_id" CurieIdentifier :> Get '[JSON] (Features, Bounds)
type EBCRAPI_get_feature_association = "get_feature_association" :> ReqBody' '[Required, Strict] '[JSON] Features2 :> Get '[JSON] FeaturesAssociation
type EBCRAPI_get_associations_to_all_features = "get_associations_to_all_features" :> ReqBody' '[Required, Strict] '[JSON] Feature1 :> Get '[JSON] AssociationToAllFeatures

type EBCRAPI = EBCRAPI_get_ids_by_feature
          :<|> EBCRAPI_get_features_by_id
          :<|> EBCRAPI_get_feature_association
          :<|> EBCRAPI_get_associations_to_all_features

proxyEBCRAPI :: Proxy EBCRAPI
proxyEBCRAPI = Proxy


