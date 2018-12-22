{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DefaultSignatures #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}

module Capabilities
  (
  ) where

import Control.Monad.Freer
import Control.Monad.Freer.Error
import Control.Monad.Freer.Reader

import qualified Data.List.NonEmpty as NonEmpty
import qualified Data.Map.Lazy as Map

import GHC.Generics hiding (Constructor)

data Kind
  = Type
  | Rgn
  | Cap
  deriving (Eq, Show, Generic)

newtype Variable = Variable Int
  deriving (Eq, Show)

newtype Name = Name Int
  deriving (Eq, Show)

newtype Location = Location Int
  deriving (Eq, Show)

data Type
  = TVar Variable
  | IntType
  | HandleType Region
  | Fun ConstrContext Capability (NonEmpty.NonEmpty Type) Region
  | TupleType (NonEmpty.NonEmpty Type) Region
  deriving (Eq, Show, Generic)

data Region
  = RVar Variable
  | RegionName Name
  deriving (Eq, Show, Generic)

data Term
  = Let Decl Term
  | If0 Value Term Term
  | App Value (NonEmpty.NonEmpty Value)
  | Halt Value
  deriving (Eq, Show)

data Decl
  = Assign Value
  | Arith BinOp Value Value
  | Alloc HValue Value
  | Proj Int Value
  | NewRgn Variable
  | FreeRgn Value
  deriving (Eq, Show)

data BinOp
  = Add
  | Sub
  | Mul
  deriving (Eq, Show)

data Value
  = Var Int
  | Int Int
  | Address Name Location
  | Handle Name
  | Inst Value Constructor
  deriving (Eq, Show)

data Constructor
  = CVar Variable
  | CType Type
  | CRegion Region
  | CCapability Capability
  deriving (Eq, Show)

data HValue
  = Fix ConstrContext Capability (NonEmpty.NonEmpty Type) Term
  | Tuple (NonEmpty.NonEmpty Value)
  deriving (Eq, Show)

data Capability
  = CapVar Variable
  | Empty
  | Singleton Region Multi
  | Join Capability Capability
  | Strip Capability
  deriving (Eq, Show, Generic)

-- Multiplicities.
data Multi
  = Unique
  | NonUnique
  deriving (Eq, Show, Generic)

data ConstrBinding
  = Bind Kind
  | Subcap Capability
  deriving (Eq, Show, Generic)

newtype ConstrContext = ConstrContext { getConstrContext :: [ConstrBinding] }
  deriving (Eq, Show, Generic)

newtype Context = Context [Type]
  deriving (Eq, Show)

newtype RegionContext = RegionContext (Map.Map Location Type)
  deriving (Eq, Show)

newtype MemoryContext = MemoryContext (Map.Map Name RegionContext)
  deriving (Eq, Show)

-- Shifts variables.
class Shift a where
  shiftAbove :: Int -> Int -> a -> a
  shift :: Int -> a -> a

  default shiftAbove :: (Generic a, GShift (Rep a)) => Int -> Int -> a -> a
  shiftAbove c d x = to $ gShiftAbove c d $ from x

  shift d x = shiftAbove 0 d x

class GShift f where
  gShiftAbove :: Int -> Int -> f a -> f a

instance GShift U1 where
  gShiftAbove _ _ U1 = U1

instance (GShift a, GShift b) => GShift (a :*: b) where
  gShiftAbove c d (x :*: y) = gShiftAbove c d x :*: gShiftAbove c d y

instance (GShift a, GShift b) => GShift (a :+: b) where
  gShiftAbove c d (L1 x) = L1 $ gShiftAbove c d x
  gShiftAbove c d (R1 x) = R1 $ gShiftAbove c d x

instance GShift a => GShift (M1 i c a) where
  gShiftAbove c d (M1 x) = M1 $ gShiftAbove c d x

instance Shift a => GShift (K1 i a) where
  gShiftAbove c d (K1 x) = K1 $ shiftAbove c d x

instance Shift Variable where
  shiftAbove c d v @ (Variable n)
    | c <= n    = Variable $ n + d
    | otherwise = v

instance Shift Name where
  -- Names are not variables, so nothing happens.
  shiftAbove _ _ = id

instance Shift Capability
instance Shift ConstrBinding
instance Shift ConstrContext
instance Shift Kind
instance Shift Multi
instance Shift Region

instance Shift a => Shift (NonEmpty.NonEmpty a) where
  shiftAbove c d x = shiftAbove c d <$> x

instance Shift a => Shift [a] where
  shiftAbove c d x = shiftAbove c d <$> x

instance Shift Type where
  shiftAbove c d (Fun cctx cap ts r) = Fun (f cctx) (g cap) (g ts) (g r)
    where
      c' = c + length (getConstrContext cctx)
      f = shiftAbove c d
      g :: Shift a => a -> a
      g = shiftAbove c' d
  shiftAbove c d ty = to $ gShiftAbove c d $ from ty

data TypeError
  = TypeMismatch Type Type
  | UnboundVariable Int
  deriving (Eq, Show)

nth :: Int -> [a] -> Maybe a
nth n xs
  | 0 <= n && n < length xs = return $ xs !! n
  | otherwise               = Nothing

lookupVar :: Members '[Reader Context, Error TypeError] r => Int -> Eff r Type
lookupVar n = do
  Context ts <- ask
  maybe (throwError $ UnboundVariable n) return $ nth n ts

class Typed a where
  type Output a
  type Effects a :: [* -> *]

  typeOf :: Members (Effects a) r => a -> Eff r (Output a)

instance Typed Value where
  type Output Value = Type
  type Effects Value = '[Reader Context, Error TypeError]

  typeOf (Var n) = lookupVar n
  typeOf (Int _) = return IntType
  typeOf (Handle name) = return $ HandleType $ RegionName name
