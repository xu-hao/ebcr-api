{-# LANGUAGE DataKinds, TypeOperators, DuplicateRecordFields, DeriveGeneric, GeneralizedNewtypeDeriving #-}
module ECBR where

import Servant.API
import Data.Map
import Data.Proxy
import GHC.Generics
import Data.Aeson (FromJSON, ToJSON, FromJSONKey, ToJSONKey)
import Web.HttpApiData

newtype Feature = Feature String deriving (Generic, Show, Ord, Eq, FromJSONKey, ToJSONKey)
newtype Value = Value String deriving (Generic, Show)
type Features = Map Feature Value

data Op = GT | LT | GE | LE | EQ | NE deriving (Generic, Show)

data FeatureInequality = FeatureInequality Feature Op Value deriving (Generic, Show)

newtype CurieIdentifier = CurieIdentifier String deriving (Generic, Show, FromHttpApiData, ToHttpApiData)

data Bounds = Bounds {
    lower_bound :: Int,
    upper_bound :: Int
} deriving (Generic, Show)

data Cohort = Cohort {
    cohort_id :: CurieIdentifier,
    cohort_size :: Bounds
} deriving (Generic, Show)

data Features2 = Features2 {
    cohort_id :: CurieIdentifier,
    feature_a :: FeatureInequality,
    feature_b :: FeatureInequality
} deriving (Generic, Show)

data Feature1 = Feature1 {
    cohort_id :: CurieIdentifier,
    feature :: FeatureInequality,
    maximum_p_value :: Double
} deriving (Generic, Show)

data FeaturesAssociation = FeaturesAssociation {
    chi_squared :: Double,
    p_value :: Double,
    feature_matrix :: ((Int,Int),(Int, Int))
} deriving (Generic, Show)

data AssociationToAllFeatures = AssociationToAllFeatures {
    corrected_chi_squared_statistics :: [Double],
    p_values :: [Double],
    feature_matrix :: [(Feature,Int,Int,Double,Double)]
} deriving (Generic, Show)

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
instance ToJSON Cohort
instance FromJSON Cohort
instance ToJSON Features2
instance FromJSON Features2
instance ToJSON Feature1
instance FromJSON Feature1
instance ToJSON FeaturesAssociation
instance FromJSON FeaturesAssociation
instance ToJSON AssociationToAllFeatures
instance FromJSON AssociationToAllFeatures

type ECBRAPI = "get_ids_by_feature" :> ReqBody' '[Required, Strict] '[JSON] Features :> Get '[JSON] Cohort
          :<|> "get_features_by_id" :> QueryParam' '[Required, Strict] "cohort_id" CurieIdentifier :> Get '[JSON] (Features, Bounds)
          :<|> "get_feature_association" :> ReqBody' '[Required, Strict] '[JSON] Features2 :> Get '[JSON] FeaturesAssociation
          :<|> "get_associations_to_all_features" :> ReqBody' '[Required, Strict] '[JSON] Feature1 :> Get '[JSON] AssociationToAllFeatures

proxyECBRAPI :: Proxy ECBRAPI
proxyECBRAPI = Proxy


