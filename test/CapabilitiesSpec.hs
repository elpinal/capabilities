module CapabilitiesSpec where

import Test.Hspec
import Test.QuickCheck

import Control.Monad
import Control.Monad.Freer
import Control.Monad.Freer.Error
import Control.Monad.Freer.Reader

import Capabilities

v0 :: Variable
v0 = Variable 0

v1 :: Variable
v1 = Variable 1

v2 :: Variable
v2 = Variable 2

instance Arbitrary Variable where
  arbitrary = liftM Variable arbitrary

instance Arbitrary Region where
  arbitrary = liftM RVar arbitrary

instance Arbitrary Multi where
  arbitrary = oneof
    [ return Unique
    , return NonUnique
    ]

instance Arbitrary Capability where
  arbitrary = sized cap'
    where cap' 0 = oneof
            [ liftM CapVar arbitrary
            , return Empty
            , liftM2 Singleton arbitrary arbitrary
            ]
          cap' n = oneof
            [ liftM CapVar arbitrary
            , return Empty
            , liftM2 Singleton arbitrary arbitrary
            , liftM2 Join sub sub
            , liftM Strip sub
            ]
            where sub = cap' (n `div` 2)

spec :: Spec
spec = do
  describe "capEqual" $ do
    it "tests whether two capabilities are equal" $ do
      capEqual Empty Empty                          `shouldBe` True
      capEqual (Join Empty $ CapVar v0) (CapVar v0) `shouldBe` True
      capEqual (Join (CapVar v0) Empty) (CapVar v0) `shouldBe` True

      let c1 = (Join (CapVar v0) (CapVar v1) `Join` CapVar v2)
      capEqual c1 (Join (CapVar v0) $ CapVar v1 `Join` CapVar v2) `shouldBe` True

      capEqual (Strip c1) (Strip c1 `Join` Strip c1) `shouldBe` True
      capEqual (Strip Empty) Empty                   `shouldBe` True

      let r1 = RVar v0
      capEqual (Strip $ Singleton r1 Unique) (Singleton r1 NonUnique)    `shouldBe` True
      capEqual (Strip $ Singleton r1 NonUnique) (Singleton r1 NonUnique) `shouldBe` True

      capEqual (Singleton r1 NonUnique) (Strip (Singleton r1 NonUnique) `Join` Strip (Singleton r1 NonUnique)) `shouldBe` True
      capEqual (Singleton r1 NonUnique) (Singleton r1 NonUnique `Join` Singleton r1 NonUnique)                 `shouldBe` True

      capEqual (Singleton r1 NonUnique) (Singleton r1 Unique `Join` Singleton r1 Unique)                 `shouldBe` False
      capEqual (Singleton r1 NonUnique) (Strip (Singleton r1 Unique) `Join` Strip (Singleton r1 Unique)) `shouldBe` True

      capEqual (Strip $ Strip $ Singleton r1 Unique) (Strip $ Singleton r1 Unique) `shouldBe` True

      let r2 = RVar v1
      capEqual (Strip $ Singleton r1 Unique `Join` Singleton r2 NonUnique) (Strip (Singleton r1 Unique) `Join` Strip (Singleton r2 NonUnique)) `shouldBe` True

      capEqual (Singleton r1 Unique) (Singleton r1 Unique `Join` Singleton r1 Unique) `shouldBe` False

    it "is reflexive" $ property $
      \x -> capEqual x x

    it "says stripped capabilities are duplicatable" $ property $
      \x -> let y = Strip x in capEqual (Join y y) y

  describe "(<:)" $
    it "tests whether a capability is a subcapability of another capability" $ do
      let subcap bs c1 c2 = run $ runError $ runReader (ConstrContext bs) $ c1 <: c2 :: Either TypeError Bool

      subcap [] Empty Empty `shouldBe` return True

      let r1 = RVar v0
      subcap [] Empty (Singleton r1 NonUnique) `shouldBe` return False

      subcap [] (Singleton r1 NonUnique) (Singleton r1 NonUnique) `shouldBe` return True
      subcap [] (Singleton r1 Unique) (Singleton r1 NonUnique)    `shouldBe` return True
      subcap [] (Singleton r1 Unique) (Singleton r1 Unique)       `shouldBe` return True
      subcap [] (Singleton r1 NonUnique) (Singleton r1 Unique)    `shouldBe` return False

      let r2 = RVar v1
      subcap [] (Singleton r1 Unique) (Singleton r1 NonUnique `Join` Singleton r1 NonUnique)    `shouldBe` return True
      subcap [] (Singleton r1 NonUnique) (Singleton r1 NonUnique `Join` Singleton r1 NonUnique) `shouldBe` return True
      subcap [] (Singleton r1 NonUnique) (Singleton r1 NonUnique `Join` Singleton r2 NonUnique) `shouldBe` return False

      subcap [] (Singleton r1 Unique `Join` Singleton r1 Unique) (Singleton r1 NonUnique)                               `shouldBe` return True
      subcap [] (Singleton r1 Unique `Join` Singleton r1 Unique) (Singleton r1 NonUnique `Join` Singleton r1 NonUnique) `shouldBe` return True
      subcap [] (Singleton r1 Unique `Join` Singleton r1 Unique) (Singleton r1 NonUnique `Join` Singleton r2 NonUnique) `shouldBe` return False

      subcap [] (CapVar v0) (Singleton r1 NonUnique)                                `shouldBe` Left (UnboundConstrVariable v0)
      subcap [Subcap $ Singleton r2 NonUnique] (CapVar v0) (Singleton r2 NonUnique) `shouldBe` return True
      subcap [Subcap $ Singleton r2 Unique] (CapVar v0) (Singleton r2 NonUnique)    `shouldBe` return True
      subcap [Subcap $ Singleton r2 Unique] (CapVar v0) (Singleton r2 Unique)       `shouldBe` return True
      subcap [Subcap $ Singleton r2 NonUnique] (CapVar v0) (Singleton r2 Unique)    `shouldBe` return False

      subcap [Subcap $ Singleton r2 NonUnique] (CapVar v0 `Join` CapVar v0) (CapVar v0)         `shouldBe` return False
      subcap [Subcap $ Singleton r2 NonUnique] (CapVar v0 `Join` CapVar v0) (Strip $ CapVar v0) `shouldBe` return True

      subcap [Subcap $ Singleton r2 NonUnique] (CapVar v0 `Join` CapVar v0) (Strip (CapVar v0) `Join` Singleton r2 NonUnique) `shouldBe` return True

      subcap [Subcap $ Singleton r2 NonUnique] (CapVar v0 `Join` Strip (CapVar v0)) (Strip (CapVar v0) `Join` Singleton r2 NonUnique) `shouldBe` return True

      subcap [] (Singleton r1 Unique `Join` Singleton r1 NonUnique) (Singleton r1 Unique `Join` Singleton r1 NonUnique) `shouldBe` return True
