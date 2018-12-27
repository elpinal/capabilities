module CapabilitiesSpec where

import Test.Hspec

import Capabilities

v0 :: Variable
v0 = Variable 0

v1 :: Variable
v1 = Variable 1

v2 :: Variable
v2 = Variable 2

spec :: Spec
spec = do
  describe "capEqual" $
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
