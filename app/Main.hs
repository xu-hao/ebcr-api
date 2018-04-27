module Main where

import Servant.Server
import Servant.Mock
import Network.Wai.Handler.Warp
import Data.Proxy
import Generic.Random
import Test.QuickCheck
import Test.QuickCheck.Gen
import ECBR

instance Arbitrary Op where
    arbitrary = genericArbitraryU

instance Arbitrary Cohort where
    arbitrary = genericArbitraryU

instance Arbitrary Bounds where
    arbitrary = do
        (Positive lower) <- arbitrary
        (Positive range) <- arbitrary
        return $ Bounds lower (lower + range)

instance Arbitrary FeatureInequality where
    arbitrary = genericArbitraryU

instance Arbitrary Feature1 where
    arbitrary = genericArbitraryU

instance Arbitrary Features2 where
    arbitrary = genericArbitraryU

instance Arbitrary AssociationToAllFeatures where
    arbitrary = genericArbitraryU

instance Arbitrary FeaturesAssociation where
    arbitrary = genericArbitraryU

instance Arbitrary Feature where
    arbitrary = Feature <$> do
        i <- choose ('A', 'Z')
        t <- listOf (choose ('a', 'z'))
        return $ i : t

instance Arbitrary Value where
    arbitrary = Value <$> listOf1 (choose ('0', '9'))

instance Arbitrary CurieIdentifier where
    arbitrary = CurieIdentifier <$> do
        h <- listOf1 (choose ('A', 'Z'))
        t <- listOf1 (choose ('0', '9'))
        return $ h ++ (':' : t)

main :: IO ()
main = run 8080 $
    serve proxyECBRAPI (mock proxyECBRAPI Proxy)
